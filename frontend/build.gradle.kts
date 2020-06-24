plugins {
  `maven-publish`
}

version = "0.0.1"
description = """
  Qwde frontend, reading stock data and presenting various functions through an isomorhpic HTTP site.
"""

task<Exec>("build.qwdeserver") {
  description = "build qwdeserver"
  commandLine("cabal", "install", "--program-suffix=.bin", "qwdeserver", "--installdir=dist-newstyle/target")
  inputs.files(fileTree("./qwdeserver"))
    .withPropertyName("sourceFiles")
    .withPathSensitivity(PathSensitivity.RELATIVE)
  outputs.dir("./dist-newstyle/target")
}

task<Exec>("build.qwdeclient") {
  description = "build stuff"
  commandLine("cabal", "build", "--ghcjs", "qwdeclient")
  inputs.files(fileTree("./qwdeclient"))
    .withPropertyName("sourceFiles")
    .withPathSensitivity(PathSensitivity.RELATIVE)
  outputs.dir("./dist-newstyle/target")
}

task("build") {
  dependsOn(tasks.withType<Exec>())
}

fun String.runCommand(workingDir: File = file("./")): String {
  val parts = this.split("\\s".toRegex())
    val proc = ProcessBuilder(*parts.toTypedArray())
    .directory(workingDir)
    .redirectOutput(ProcessBuilder.Redirect.PIPE)
    .redirectError(ProcessBuilder.Redirect.PIPE)
    .start()

    proc.waitFor(1, TimeUnit.MINUTES)
    return proc.inputStream.bufferedReader().readText().trim()
}


// TODO: will not work if > 1 file is found
val qwdeserverPath = "find dist-newstyle/target -name qwdeserver.bin".runCommand()
val qwdeserverExec = file(qwdeserverPath)
val qwdeclientPath = "find dist-newstyle/ -name all.js".runCommand()
val qwdeclientExec = file(qwdeclientPath)
val haskell by configurations.creating
val qwdeserverArtifact = artifacts.add("haskell", qwdeserverExec) {
  type = "binary"
  builtBy("build.qwdeserver")
  classifier = "prod"
  extension = "bin"
  name = "qwdeserver"
}
val qwdeclientArtifact = artifacts.add("haskell", qwdeclientExec) {
  type = "js"
  builtBy("build.qwdeclient")
}
val repoUser: String by project
val repoPassword: String by project

publishing {
  publications {
    create<MavenPublication>("qwdeclient") {
      groupId = "qwde.frontend"
      description = "Haskell-to-Javascript code that lets you run an isomorphic website"
      artifact(qwdeclientArtifact)
      artifactId = "client"
      pom {
        name.set("qwdeclient")
        description.set("desc")
        url.set("http://qwde.no")
        developers {
          developer {
            id.set("andsild")
            name.set("Anders")
            email.set("trolo@lol.lol")
          }
        }
      }
    }
    create<MavenPublication>("qwdeserver") {
      groupId = "qwde.frontend"
      description = "Haskell binary to run isomorhpic HTTP frontend"
      artifact(qwdeserverArtifact)
      artifactId = "server"
      pom {
        name.set("qwdeserver")
        description.set("Haskell binary to run isomorhpic HTTP frontend")
        url.set("http://qwde.no")
        developers {
          developer {
            id.set("andsild")
            name.set("Anders")
            email.set("trolo@lol.lol")
          }
        }
      }
    }
  }
  repositories {
    maven {
      url = uri("http://qwde.no:8876/repository/internal/")
      credentials {
          // Store in ~/.gradle/gradle.properties
          username = "admin"
          password = repoPassword
      }
    }
  }
}

