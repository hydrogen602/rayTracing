package rayTracing.data

import rayTracing._

class Config(xmlFileName: String) {
    val config = xml.XML.loadFile(xmlFileName)

    val side: Int = (config \ "side").text.toInt
    val up: Vect3 = Main.stringToVect((config \ "upVector").text)
    val forward: Vect3 = Main.stringToVect((config \ "forwardVector").text)
    val raySource: Vect3 = Main.stringToVect((config \ "raySource").text)

    def generateLightSource(n: xml.Node): LightSource = {
        LightSource(Main.stringToVect(n.text), Main.stringToDColor((n \ "@color").text))
    }

    val lightSources: List[LightSource] = (config \ "lightSource").map(generateLightSource).toList
}

object Config {
    def apply(xmlFileName: String): Config = {
        new Config(xmlFileName)
    }
}