import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.control.Button
import scalafx.scene.image.Image

import javafx.embed.swing
import scalafx.scene.image.{Image, WritableImage, ImageView}

import java.awt.image.BufferedImage

val width = 50
val height = 100

val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

for (i <- 0 until width; j <- 0 until height) {
    img.setRGB(i, j, 0xffff00)
}

val w: WritableImage = new WritableImage(width, height)

val iv: ImageView = new ImageView(w)

val p = swing.SwingFXUtils.toFXImage(img, w)

val app = new JFXApp {
    stage = new JFXApp.PrimaryStage {
        title = "First GUI"
        scene = new Scene(width.toDouble, height.toDouble) {
            //fill = Color.Coral
            //val button = new Button("Click me!")
            content = iv
        }
    }
}

app.main(args)