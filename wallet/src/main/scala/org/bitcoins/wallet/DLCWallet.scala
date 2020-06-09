package org.bitcoins.wallet

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.currency._
import org.bitcoins.core.hd.{AddressType, HDChainType}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{Bech32Address, BlockStamp}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.crypto._
import org.bitcoins.dlc._
import org.bitcoins.wallet.models._

import scala.concurrent.Future

abstract class DLCWallet extends Wallet {

  private def initDLC(
      eventId: Sha256DigestBE,
      isInitiator: Boolean): Future[DLCDb] = {
    dlcDAO.findByEventId(eventId).flatMap {
      case Some(dlcDb) =>
        Future.successful(dlcDb)
      case None =>
        for {
          account <- getDefaultAccountForType(AddressType.SegWit)
          nextIndex <- getNextAvailableIndex(account, HDChainType.External)
          dlc = {
            DLCDb(
              eventId = eventId,
              isInitiator = isInitiator,
              account = account.hdAccount,
              keyIndex = nextIndex,
              refundSigOpt = None,
              oracleSigOpt = None
            )
          }
          _ <- writeDLCKeysToAddressDb(account, nextIndex)
          writtenDLC <- dlcDAO.create(dlc)
        } yield writtenDLC
    }
  }

  private def updateDLCOracleSig(
      eventId: Sha256DigestBE,
      sig: SchnorrDigitalSignature): Future[DLCDb] = {
    dlcDAO.findByEventId(eventId).flatMap {
      case Some(dlcDb) =>
        dlcDAO.update(dlcDb.copy(oracleSigOpt = Some(sig)))
      case None =>
        Future.failed(
          new NoSuchElementException(
            s"No DLC found with that eventId ${eventId.hex}"))
    }
  }

  private def writeDLCKeysToAddressDb(
      account: AccountDb,
      index: Int): Future[Vector[AddressDb]] = {
    for {
      zero <- getAddress(account, HDChainType.External, index)
      one <- getAddress(account, HDChainType.External, index + 1)
      two <- getAddress(account, HDChainType.External, index + 2)
    } yield {
      Vector(zero, one, two)
    }
  }

  /**
    * Creates a DLCOffer, if one has already been created
    * with the given parameters then that one will be returned instead.
    *
    * This is the first step of the initiator
    */
  override def createDLCOffer(
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[FeeUnit],
      locktime: UInt32,
      refundLocktime: UInt32): Future[DLCOffer] = {
    logger.debug("Calculating relevant wallet data for DLC Offer")

    val timeouts = DLCTimeouts(DLCTimeouts.DEFAULT_PENALTY_TIMEOUT,
                               BlockStamp(locktime.toInt),
                               BlockStamp(refundLocktime.toInt))

    val feeRate =
      feeRateOpt.getOrElse(getFeeRate)
    val satoshisPerVirtualByte = SatoshisPerVirtualByte(feeRate.currencyUnit)

    val eventId = DLCMessage.calcEventId(oracleInfo, contractInfo, timeouts)

    logger.debug(s"Checking if DLC Offer has already been made ($eventId)")
    for {
      dlc <- initDLC(eventId = eventId, isInitiator = true)
      fundingInputs <- dlcInputsDAO.findByEventId(eventId, isInitiator = true)
      dlcOfferDbOpt <- dlcOfferDAO.findByEventId(eventId)
      dlcOffer <- dlcOfferDbOpt match {
        case Some(dlcOfferDb) =>
          logger.debug(
            s"DLC Offer ($eventId) has already been made, returning offer")

          val inputRefs = fundingInputs.map(_.toOutputReference)
          val offer = dlcOfferDb.toDLCOffer(inputRefs)
          Future.successful(offer)
        case None =>
          createNewDLCOffer(
            dlc = dlc,
            collateral = collateral,
            oracleInfo = oracleInfo,
            contractInfo = contractInfo,
            feeRate = satoshisPerVirtualByte,
            timeouts = timeouts
          )
      }
    } yield dlcOffer
  }

  private def createNewDLCOffer(
      dlc: DLCDb,
      collateral: CurrencyUnit,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts): Future[DLCOffer] = {
    for {
      accountOpt <- accountDAO.findByAccount(dlc.account)
      account = accountOpt.get
      (txBuilder, spendingInfos) <- fundRawTransactionInternal(
        destinations = Vector(TransactionOutput(collateral, EmptyScriptPubKey)),
        feeRate = feeRate,
        fromAccount = account,
        keyManagerOpt = Some(keyManager),
        markAsReserved = true
      )
      utxos = spendingInfos.map(_.outputReference)

      changeSPK = txBuilder.finalizer.changeSPK
        .asInstanceOf[WitnessScriptPubKey]
      network = networkParameters.asInstanceOf[BitcoinNetwork]
      changeAddr = Bech32Address(changeSPK, network)

      dlcPubKeys = DLCPublicKeys.fromExtPubKeyAndIndex(account.xpub,
                                                       dlc.keyIndex,
                                                       network)

      _ = logger.debug(
        s"DLC Offer data collected, creating database entry, ${dlc.eventId.hex}")

      dlcOfferDb = DLCOfferDb(
        eventId = dlc.eventId,
        network = network,
        oraclePubKey = oracleInfo.pubKey,
        oracleRValue = oracleInfo.rValue,
        contractInfo = contractInfo,
        penaltyTimeout = timeouts.penaltyTimeout,
        contractMaturity = timeouts.contractMaturity,
        contractTimeout = timeouts.contractTimeout,
        fundingKey = dlcPubKeys.fundingKey,
        toLocalCETKey = dlcPubKeys.toLocalCETKey,
        finalAddress = dlcPubKeys.finalAddress,
        totalCollateral = collateral,
        feeRate = feeRate,
        changeAddress = changeAddr
      )

      dlcInputs = utxos.map(
        outRef =>
          DLCFundingInputDb(eventId = dlc.eventId,
                            isInitiator = true,
                            outPoint = outRef.outPoint,
                            output = outRef.output,
                            sigs = Vector.empty))

      _ <- dlcInputsDAO.createAll(dlcInputs)
      _ <- dlcOfferDAO.create(dlcOfferDb)
    } yield {
      dlcOfferDb.toDLCOffer(utxos)
    }
  }

  /**
    * Creates a DLCAccept from the default Segwit account from a given offer, if one has already been
    * created with the given parameters then that one will be returned instead.
    *
    * This is the first step of the recipient
    */
  override def acceptDLCOffer(offer: DLCOffer): Future[DLCAccept] = {
    logger.debug("Calculating relevant wallet data for DLC Accept")

    val eventId =
      DLCMessage.calcEventId(offer.oracleInfo,
                             offer.contractInfo,
                             offer.timeouts)

    val collateral = offer.contractInfo.values.max - offer.totalCollateral

    logger.debug(s"Checking if Accept ($eventId) has already been made")
    for {
      dlc <- initDLC(eventId = eventId, isInitiator = false)
      accountOpt <- accountDAO.findByAccount(dlc.account)
      fundingInputs <- dlcInputsDAO.findByEventId(eventId, isInitiator = false)
      outcomeSigDbs <- dlcSigsDAO.findByEventId(eventId)
      dlcAcceptDbOpt <- dlcAcceptDAO.findByEventId(eventId)
      dlcAccept <- dlcAcceptDbOpt match {
        case Some(dlcAcceptDb) =>
          logger.debug(
            s"DLC Accept ($eventId) has already been made, returning accept")
          val inputRefs = fundingInputs.map(_.toOutputReference)
          val outcomeSigs = outcomeSigDbs.map(_.toTuple).toMap
          val accept = dlcAcceptDb.toDLCAccept(inputRefs, outcomeSigs)
          Future.successful(accept)
        case None =>
          createNewDLCAccept(dlc, accountOpt.get, collateral, offer)
      }
    } yield dlcAccept
  }

  private def createNewDLCAccept(
      dlc: DLCDb,
      account: AccountDb,
      collateral: CurrencyUnit,
      offer: DLCOffer): Future[DLCAccept] = {
    for {
      (txBuilder, spendingInfos) <- fundRawTransactionInternal(
        destinations = Vector(TransactionOutput(collateral, EmptyScriptPubKey)),
        feeRate = offer.feeRate,
        fromAccount = account,
        keyManagerOpt = Some(keyManager),
        markAsReserved = true
      )
      network = networkParameters.asInstanceOf[BitcoinNetwork]

      utxos = spendingInfos.map(_.outputReference)

      changeSPK = txBuilder.finalizer.changeSPK.asInstanceOf[P2WPKHWitnessSPKV0]
      changeAddr = Bech32Address(changeSPK, network)

      client = DLCClient.fromOffer(
        offer,
        keyManager.rootExtPrivKey
          .deriveChildPrivKey(account.hdAccount), // todo change to a ExtSign.deriveAndSignFuture
        dlc.keyIndex,
        spendingInfos,
        collateral,
        changeSPK,
        network
      )
      cetSigs <- client.createCETSigs
      dlcPubKeys = DLCPublicKeys.fromExtPubKeyAndIndex(account.xpub,
                                                       dlc.keyIndex,
                                                       network)

      _ = logger.debug(
        s"DLC Accept data collected, creating database entry, ${dlc.eventId.hex}")

      dlcAcceptDb = DLCAcceptDb(
        eventId = dlc.eventId,
        fundingKey = dlcPubKeys.fundingKey,
        toLocalCETKey = dlcPubKeys.toLocalCETKey,
        finalAddress = dlcPubKeys.finalAddress,
        totalCollateral = collateral,
        refundSig = cetSigs.refundSig,
        changeAddress = changeAddr
      )

      sigsDbs = cetSigs.outcomeSigs.map(sig =>
        DLCCETSignatureDb(dlc.eventId, sig._1, sig._2))

      dlcOfferDb = DLCOfferDb.fromDLCOffer(offer, network)

      offerInputs = offer.fundingInputs.map(
        outRef =>
          DLCFundingInputDb(eventId = dlc.eventId,
                            isInitiator = true,
                            outPoint = outRef.outPoint,
                            output = outRef.output,
                            sigs = Vector.empty))
      acceptInputs = utxos.map(
        outRef =>
          DLCFundingInputDb(eventId = dlc.eventId,
                            isInitiator = false,
                            outPoint = outRef.outPoint,
                            output = outRef.output,
                            sigs = Vector.empty))

      _ <- dlcInputsDAO.createAll(offerInputs ++ acceptInputs)
      _ <- dlcOfferDAO.create(dlcOfferDb)
      _ <- dlcAcceptDAO.create(dlcAcceptDb)
      _ <- dlcSigsDAO.createAll(sigsDbs.toVector)
    } yield {
      dlcAcceptDb.toDLCAccept(utxos, cetSigs.outcomeSigs)
    }
  }

  def registerDLCAccept(accept: DLCAccept): Future[DLCAcceptDb] = {
    dlcOfferDAO.findByEventId(accept.eventId).flatMap {
      case Some(_) =>
        logger.debug(
          s"DLC Offer (${accept.eventId.hex}) found, adding accept data")

        val dlcAcceptDb = DLCAcceptDb.fromDLCAccept(accept)
        val acceptInputs = accept.fundingInputs.map(
          outRef =>
            DLCFundingInputDb(eventId = accept.eventId,
                              isInitiator = false,
                              outPoint = outRef.outPoint,
                              output = outRef.output,
                              sigs = Vector.empty))
        val sigsDbs = accept.cetSigs.outcomeSigs
          .map(sig => DLCCETSignatureDb(accept.eventId, sig._1, sig._2))
          .toVector

        for {
          _ <- dlcInputsDAO.upsertAll(acceptInputs)
          _ <- dlcSigsDAO.upsertAll(sigsDbs)
          acceptDb <- dlcAcceptDAO.upsert(dlcAcceptDb)
        } yield acceptDb
      case None =>
        throw new RuntimeException(
          s"No DLC Offer found with corresponding eventId ${accept.eventId}, this wallet did not create the corresponding offer")
    }
  }

  /**
    * Creates signatures for the DLCs CETs and Funding Inputs
    *
    * This is the second step of the initiator
    */
  override def signDLC(accept: DLCAccept): Future[DLCSign] = {
    for {
      _ <- registerDLCAccept(accept)
      dlcOpt <- dlcDAO.findByEventId(accept.eventId)
      offerOpt <- dlcOfferDAO.findByEventId(accept.eventId)
      offer = offerOpt.get // Safe, we throw in registerDLCAccept if it is None
      dlc = dlcOpt.get
      fundingInputs <- dlcInputsDAO.findByEventId(accept.eventId,
                                                  isInitiator = true)
      spendingInfoDbs <- listUtxos(fundingInputs.map(_.outPoint))
      spendingInfos = spendingInfoDbs.flatMap(
        _.toUTXOInfo(keyManager).toSingles)
      client = DLCClient.fromOfferAndAccept(
        offer.toDLCOffer(fundingInputs.map(_.toOutputReference)),
        accept,
        keyManager.rootExtPrivKey.deriveChildPrivKey(dlc.account),
        dlc.keyIndex,
        spendingInfos.map(_.toScriptSignatureParams),
        offer.network
      )
      cetSigs <- client.createCETSigs
      fundingSigs <- client.createFundingTransactionSigs()
      updatedDLCDb = dlc.copy(refundSigOpt = Some(cetSigs.refundSig))
      _ <- dlcDAO.update(updatedDLCDb)
    } yield {
      DLCSign(cetSigs, fundingSigs, accept.eventId)
    }
  }

  def verifyCETSigs(sign: DLCSign): Future[Boolean] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigs) <- getAllDLCData(
        sign.eventId)

      client <- clientFromDb(dlcDb,
                             dlcOffer,
                             dlcAccept,
                             fundingInputs,
                             outcomeSigs)
    } yield {
      val correctNumberOfSigs = sign.cetSigs.outcomeSigs.size == client.outcomes.size

      correctNumberOfSigs && sign.cetSigs.outcomeSigs.foldLeft(true) {
        case (ret, (outcome, sig)) =>
          ret && client.verifyCETSig(outcome, sig)
      }
    }
  }

  def verifyRefundSig(sign: DLCSign): Future[Boolean] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigs) <- getAllDLCData(
        sign.eventId)

      client <- clientFromDb(dlcDb,
                             dlcOffer,
                             dlcAccept,
                             fundingInputs,
                             outcomeSigs)
    } yield client.verifyRefundSig(sign.cetSigs.refundSig)
  }

  def verifyFundingSigs(
      inputs: Vector[DLCFundingInputDb],
      sign: DLCSign): Future[Boolean] = {
    if (inputs.count(!_.isInitiator) == sign.fundingSigs.keys.size) {
      for {
        (dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigs) <- getAllDLCData(
          sign.eventId)

        client <- clientFromDb(dlcDb,
                               dlcOffer,
                               dlcAccept,
                               fundingInputs,
                               outcomeSigs)

      } yield client.verifyRemoteFundingSigs(sign.fundingSigs)
    } else {
      logger.info(
        "Funding Signatures provided did not have the correct amount of inputs")
      Future.successful(false)
    }
  }

  /** Takes a DLCSign an inserts the funding signatures into the database
    * This is the only way one should insert sigs to the database */
  def addFundingSigs(sign: DLCSign): Future[Vector[DLCFundingInputDb]] = {
    for {
      inputs <- dlcInputsDAO.findByEventId(sign.eventId)
      isValid <- verifyFundingSigs(inputs, sign)
      _ = if (!isValid)
        throw new IllegalArgumentException(
          s"Funding Signatures provided are not valid! got ${sign.fundingSigs}")
      updatedInputs = sign.fundingSigs.map {
        case (outPoint, sigs) =>
          inputs.find(_.outPoint == outPoint) match {
            case Some(inputDb) =>
              inputDb.copy(sigs = sigs)
            case None =>
              throw new NoSuchElementException(
                s"Received signature for outPoint (${outPoint.hex}) that does not correspond to this eventId (${sign.eventId.hex})")
          }
      }
      written <- dlcInputsDAO.upsertAll(updatedInputs.toVector)
    } yield written
  }

  /**
    * Inputs the received signatures for a DLC into our database
    *
    * This is the second step of the recipient
    */
  override def addDLCSigs(sign: DLCSign): Future[DLCDb] = {
    for {
      dlcDb <- dlcDAO.findByEventId(sign.eventId).flatMap {
        case Some(dlc) =>
          val newDLCDb = dlc.copy(
            refundSigOpt = Some(sign.cetSigs.refundSig)
          )
          val sigsDbs = sign.cetSigs.outcomeSigs
            .map(sig => DLCCETSignatureDb(sign.eventId, sig._1, sig._2))
            .toVector

          for {
            isRefundSigValid <- verifyRefundSig(sign)
            _ = if (!isRefundSigValid)
              throw new IllegalArgumentException(
                s"Refund sig provided is not valid! got ${sign.cetSigs.refundSig}")
            dlcDb <- dlcDAO.update(newDLCDb)

            isCETSigsValid <- verifyCETSigs(sign)
            _ = if (!isCETSigsValid)
              throw new IllegalArgumentException(
                s"CET sigs provided are not valid! got ${sign.cetSigs.outcomeSigs}")
            _ <- dlcSigsDAO.createAll(sigsDbs)

            _ <- addFundingSigs(sign)
          } yield dlcDb
        case None =>
          Future.failed(
            new NoSuchElementException(
              s"No DLC found with corresponding eventId ${sign.eventId}"))
      }
    } yield dlcDb
  }

  private def getAllDLCData(eventId: Sha256DigestBE): Future[
    (
        DLCDb,
        DLCOfferDb,
        DLCAcceptDb,
        Vector[DLCFundingInputDb],
        Vector[DLCCETSignatureDb])] = {
    for {
      dlcDbOpt <- dlcDAO.findByEventId(eventId)
      dlcDb = dlcDbOpt.get
      dlcOfferOpt <- dlcOfferDAO.findByEventId(eventId)
      dlcOffer = dlcOfferOpt.get
      dlcAcceptOpt <- dlcAcceptDAO.findByEventId(eventId)
      dlcAccept = dlcAcceptOpt.get
      fundingInputs <- dlcInputsDAO.findByEventId(eventId)
      outcomeSigs <- dlcSigsDAO.findByEventId(eventId)
    } yield (dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigs)
  }

  private def clientFromDb(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputs: Vector[DLCFundingInputDb],
      outcomeSigDbs: Vector[DLCCETSignatureDb]): Future[DLCClient] = {
    val extPrivKey = keyManager.rootExtPrivKey.deriveChildPrivKey(dlcDb.account)

    val offerInputs =
      fundingInputs.filter(_.isInitiator).map(_.toOutputReference)
    val acceptInputs =
      fundingInputs.filterNot(_.isInitiator).map(_.toOutputReference)

    val outcomeSigs = outcomeSigDbs.map(_.toTuple).toMap

    val offer = dlcOffer.toDLCOffer(offerInputs)
    val accept = dlcAccept.toDLCAccept(acceptInputs, outcomeSigs)

    val (setupMsg, remoteSetupMsg) = if (dlcDb.isInitiator) {
      (offer, accept)
    } else {
      (accept, offer)
    }

    val utxosF = listUtxos(setupMsg.fundingInputs.map(_.outPoint))
      .map(_.map(info => info.toUTXOInfo(keyManager)))

    utxosF.map { fundingUtxos =>
      val timeouts = DLCTimeouts(dlcOffer.penaltyTimeout,
                                 dlcOffer.contractMaturity,
                                 dlcOffer.contractTimeout)

      val outcomes =
        if (dlcDb.isInitiator) dlcOffer.contractInfo
        else
          ContractInfo(offer.contractInfo.map {
            case (hash, amt) =>
              (hash,
               (accept.totalCollateral + offer.totalCollateral - amt).satoshis)
          })

      DLCClient(
        outcomes = outcomes,
        oraclePubKey = dlcOffer.oraclePubKey,
        preCommittedR = dlcOffer.oracleRValue,
        isInitiator = dlcDb.isInitiator,
        extPrivKey = extPrivKey,
        nextAddressIndex = dlcDb.keyIndex,
        remotePubKeys = remoteSetupMsg.pubKeys,
        input = setupMsg.totalCollateral,
        remoteInput = remoteSetupMsg.totalCollateral,
        fundingUtxos = fundingUtxos,
        remoteFundingInputs = remoteSetupMsg.fundingInputs,
        timeouts = timeouts,
        feeRate = dlcOffer.feeRate,
        changeSPK = setupMsg.changeAddress.scriptPubKey,
        remoteChangeSPK = remoteSetupMsg.changeAddress.scriptPubKey,
        network = dlcOffer.network
      )
    }
  }

  private def clientAndSetupFromDb(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputs: Vector[DLCFundingInputDb],
      outcomeSigDbs: Vector[DLCCETSignatureDb]): Future[(DLCClient, SetupDLC)] = {

    clientFromDb(dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigDbs)
      .flatMap { client =>
        val outcomeSigs = outcomeSigDbs.map(_.toTuple).toMap

        val setupF = if (dlcDb.isInitiator) {
          // TODO: Note that the funding tx in this setup is not signed
          val cetSigs = CETSignatures(outcomeSigs, dlcAccept.refundSig)
          client.setupDLCOffer(
            Future.successful(cetSigs),
            (_, _) => FutureUtil.unit,
            Future.successful(client.createUnsignedFundingTransaction))
        } else {
          val cetSigs = CETSignatures(outcomeSigs, dlcDb.refundSigOpt.get)
          val fundingSigs =
            fundingInputs
              .filter(_.isInitiator)
              .map(input => (input.outPoint, input.sigs))
              .toMap
          client.setupDLCAccept(
            _ => FutureUtil.unit,
            Future.successful((cetSigs, FundingSignatures(fundingSigs))))
        }

        setupF.map((client, _))
      }
  }

  override def initDLCMutualClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[DLCMutualCloseSig] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigs) <- getAllDLCData(
        eventId)

      client <- clientFromDb(dlcDb,
                             dlcOffer,
                             dlcAccept,
                             fundingInputs,
                             outcomeSigs)

      (payout, _) = client.getPayouts(oracleSig)
      _ = if (payout <= 0.satoshis)
        throw new UnsupportedOperationException(
          "Cannot execute a losing outcome")

      sigMessage <- client.createMutualCloseSig(eventId, oracleSig)
    } yield sigMessage
  }

  override def executeDLCUnilateralClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[
    (Transaction, Option[Transaction])] = {
    for {
      _ <- updateDLCOracleSig(eventId, oracleSig)

      (dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigs) <- getAllDLCData(
        eventId)

      (client, setup) <- clientAndSetupFromDb(dlcDb,
                                              dlcOffer,
                                              dlcAccept,
                                              fundingInputs,
                                              outcomeSigs)

      (payout, _) = client.getPayouts(oracleSig)
      _ = if (payout <= 0.satoshis)
        throw new UnsupportedOperationException(
          "Cannot execute a losing outcome")

      outcome <- client.executeUnilateralDLC(setup, oracleSig)
    } yield {
      outcome match {
        case closing: UnilateralDLCOutcomeWithClosing =>
          (closing.cet, Some(closing.closingTx))
        case _: UnilateralDLCOutcomeWithDustClosing =>
          (outcome.cet, None)
      }
    }
  }

  override def executeRemoteUnilateralDLC(
      eventId: Sha256DigestBE,
      cet: Transaction): Future[Option[Transaction]] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigs) <- getAllDLCData(
        eventId)

      (client, setup) <- clientAndSetupFromDb(dlcDb,
                                              dlcOffer,
                                              dlcAccept,
                                              fundingInputs,
                                              outcomeSigs)

      newAddr <- getNewAddress(AddressType.SegWit)

      outcome <- client.executeRemoteUnilateralDLC(
        setup,
        cet,
        newAddr.scriptPubKey.asInstanceOf[WitnessScriptPubKey])
    } yield {
      outcome match {
        case closing: UnilateralDLCOutcomeWithClosing =>
          Some(closing.closingTx)
        case _: UnilateralDLCOutcomeWithDustClosing =>
          None
      }
    }
  }

  override def acceptDLCMutualClose(
      mutualCloseSig: DLCMutualCloseSig): Future[Transaction] = {
    for {
      _ <- updateDLCOracleSig(mutualCloseSig.eventId, mutualCloseSig.oracleSig)
      (dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigs) <- getAllDLCData(
        mutualCloseSig.eventId)

      client <- clientFromDb(dlcDb,
                             dlcOffer,
                             dlcAccept,
                             fundingInputs,
                             outcomeSigs)

      tx <- client.createMutualCloseTx(mutualCloseSig.oracleSig,
                                       mutualCloseSig.mutualSig)
    } yield tx
  }

  override def getDLCFundingTx(eventId: Sha256DigestBE): Future[Transaction] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigs) <- getAllDLCData(
        eventId)

      (_, setup) <- clientAndSetupFromDb(dlcDb,
                                         dlcOffer,
                                         dlcAccept,
                                         fundingInputs,
                                         outcomeSigs)
    } yield setup.fundingTx
  }

  override def executeDLCForceClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[
    (Transaction, Option[Transaction])] =
    executeDLCUnilateralClose(eventId, oracleSig)

  override def claimDLCRemoteFunds(
      eventId: Sha256DigestBE,
      forceCloseTx: Transaction): Future[Option[Transaction]] =
    executeRemoteUnilateralDLC(eventId, forceCloseTx)

  override def executeDLCRefund(
      eventId: Sha256DigestBE): Future[(Transaction, Option[Transaction])] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigs) <- getAllDLCData(
        eventId)

      (client, setup) <- clientAndSetupFromDb(dlcDb,
                                              dlcOffer,
                                              dlcAccept,
                                              fundingInputs,
                                              outcomeSigs)

      outcome <- client.executeRefundDLC(setup)
    } yield {
      outcome match {
        case closing: RefundDLCOutcomeWithClosing =>
          (closing.refundTx, Some(closing.closingTx))
        case _: RefundDLCOutcomeWithDustClosing =>
          (outcome.refundTx, None)
      }
    }
  }

  override def claimDLCPenaltyFunds(
      eventId: Sha256DigestBE,
      forceCloseTx: Transaction): Future[Option[Transaction]] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigs) <- getAllDLCData(
        eventId)

      (client, setup) <- clientAndSetupFromDb(dlcDb,
                                              dlcOffer,
                                              dlcAccept,
                                              fundingInputs,
                                              outcomeSigs)

      outcome <- client.executeJusticeDLC(setup, forceCloseTx)
    } yield {
      outcome match {
        case closing: UnilateralDLCOutcomeWithClosing =>
          Some(closing.closingTx)
        case _: UnilateralDLCOutcomeWithDustClosing =>
          None
      }
    }
  }
}