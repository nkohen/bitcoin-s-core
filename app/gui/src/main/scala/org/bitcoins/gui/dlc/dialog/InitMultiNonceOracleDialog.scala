package org.bitcoins.gui.dlc.dialog

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.MultiNonceContractInfo
import org.bitcoins.commons.jsonmodels.dlc.{
  OutcomeValueFunction,
  OutcomeValuePoint
}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.util.GUIUtil.setNumericInput
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, HBox, VBox}
import scalafx.stage.Window

object InitMultiNonceOracleDialog {

  def showAndWait(parentWindow: Window): Option[MultiNonceContractInfo] = {
    val dialog =
      new Dialog[Option[MultiNonceContractInfo]]() {
        initOwner(parentWindow)
        title = "Initialize Demo Oracle"
        headerText = "Enter contract interpolation points"
      }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets
    dialog.resizable = true

    val baseTF = new TextField() {
      text = "2"
    }

    val numDigitsTF = new TextField()

    val totalCollateralTF = new TextField() {
      promptText = "Satoshis"
    }
    setNumericInput(baseTF)
    setNumericInput(totalCollateralTF)

    val pointMap: scala.collection.mutable.Map[
      Int,
      (TextField, TextField, CheckBox)] =
      scala.collection.mutable.Map.empty

    var nextPointRow: Int = 2
    val pointGrid: GridPane = new GridPane {
      alignment = Pos.Center
      padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
      hgap = 5
      vgap = 5

      add(new Label("Outcome"), 0, 0)
      add(new Label("Payout"), 1, 0)
      add(new Label("Endpoint"), 2, 0)
    }

    def addPointRow(): Unit = {

      val xTF = new TextField() {
        promptText = "Outcome (base 10)"
      }
      val yTF = new TextField() {
        promptText = "Satoshis"
      }
      val endPointBox = new CheckBox() {
        selected = true
      }
      setNumericInput(xTF)
      setNumericInput(yTF)

      val row = nextPointRow
      pointMap.addOne((row, (xTF, yTF, endPointBox)))

      pointGrid.add(xTF, 0, row)
      pointGrid.add(yTF, 1, row)
      pointGrid.add(new Label("Endpoint:"), 2, row)
      pointGrid.add(endPointBox, 3, row)

      nextPointRow += 1
      dialog.dialogPane().getScene.getWindow.sizeToScene()
    }

    addPointRow()
    addPointRow()

    val addPointButton: Button = new Button("+") {
      onAction = _ => addPointRow()
    }

    dialog.dialogPane().content = new VBox() {
      padding = Insets(20, 10, 10, 10)
      spacing = 10
      alignment = Pos.Center

      val eventDataGrid: GridPane = new GridPane {
        padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
        hgap = 5
        vgap = 5

        add(new Label("Base"), 0, 0)
        add(baseTF, 1, 0)
        add(new Label("Num Digits"), 0, 1)
        add(numDigitsTF, 1, 1)
        add(new Label("Total Collateral"), 0, 2)
        add(totalCollateralTF, 1, 2)
      }

      val outcomes: Node = new VBox {
        alignment = Pos.Center

        val label: HBox = new HBox {
          alignment = Pos.Center
          spacing = 10
          children = Vector(new Label("Points"), addPointButton)
        }
        children = Vector(label, pointGrid)
      }

      children = Vector(eventDataGrid, new Separator(), outcomes)
    }
    // Enable/Disable OK button depending on whether all data was entered.
    val okButton = dialog.dialogPane().lookupButton(ButtonType.OK)
    // Simple validation that sufficient data was entered
    okButton.disable <== baseTF.text.isEmpty || totalCollateralTF.text.isEmpty

    Platform.runLater(numDigitsTF.requestFocus())

    // When the OK button is clicked, convert the result to a CreateDLCOffer.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        val base = baseTF.text.value.toInt
        val numDigits = numDigitsTF.text.value.toInt
        val totalCollateral = Satoshis(totalCollateralTF.text.value.toLong)

        val points = pointMap.values.toVector
        val outcomesValuePoints = points.flatMap {
          case (xTF, yTF, checkBox) =>
            if (xTF.text.value.nonEmpty && yTF.text.value.nonEmpty) {
              val x = xTF.text.value.toLong
              val y = yTF.text.value.toLong
              Some(OutcomeValuePoint(x, Satoshis(y), checkBox.selected.value))
            } else {
              None
            }
        }

        val func = OutcomeValueFunction(outcomesValuePoints)
        val contractInfo =
          MultiNonceContractInfo(func, base, numDigits, totalCollateral)

        Some(contractInfo)
      } else None

    dialog.showAndWait() match {
      case Some(Some(contractInfo: MultiNonceContractInfo)) =>
        Some(contractInfo)
      case Some(_) | None => None
    }
  }
}
