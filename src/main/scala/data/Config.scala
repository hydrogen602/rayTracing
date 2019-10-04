package rayTracing.data

import rayTracing._

class Config(xmlFileName: String) {
    val config = xml.XML.loadFile(xmlFileName)

    val side: Int = (config \ "side").text.toInt
    val up: Vect3 = Main.stringToVect((config \ "upVector").text)
    val forward: Vect3 = Main.stringToVect((config \ "forwardVector").text)
    val raySource: Vect3 = Main.stringToVect((config \ "raySource").text)
}

object Config {
    def apply(xmlFileName: String): Config = {
        new Config(xmlFileName)
    }
}