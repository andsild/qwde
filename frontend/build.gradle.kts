plugins {
  `maven-publish`
}

version = "0.0.1"
description = """
  Qwde frontend, reading stock data and presenting various functions through an isomorhpic HTTP site.
"""

task<Exec>("build.cabal.qwdeserver") {
  description = "build qwdeserver"
  commandLine("nix-build")
  inputs.files(fileTree("./qwdeserver"), fileTree("./qwdeclient"), fileTree("./qwdeutil"), , fileTree("./qwdeshared"))
    .withPropertyName("sourceFiles")
    .withPathSensitivity(PathSensitivity.RELATIVE)
  outputs.dir("./result-5/bin/")
}

task("build") {
  dependsOn(tasks.withType<Exec>())
}

task<Tar>("zipper") {
  dependsOn("build.cabal.qwdeserver")
  compression = Compression.GZIP
  from("./") {
    includeEmptyDirs = false
    include(setOf("*result-5/bin/qwdeserver", "result-4/bin/qwdeclient.jsexe/all.js"))

    filesMatching("*qwdeserver") {
      path = "qwdefrontend/"
      name = "qwdefrontend/qwdeserver.bin"
      }
    filesMatching("**all.js") {
      path = "qwdefrontend/"
      name = "qwdefrontend/all.js"
      }
  }
  into("qwdefrontend")
  setArchiveName("qwdefrontend.tar.gz")
  setDestinationDir(File("target/"))
}

val haskell by configurations.creating
val repoUser: String? by project
val repoPassword: String? by project
val artifact = artifacts.add("haskell", file("target/qwdefrontend.tar.gz")) {
  type = "tarball"
  builtBy("zipper")
  classifier = "prod"
  extension = "tar.gz"
  name = "qwdeserver"
}

publishing {
  publications {
    create<MavenPublication>("qwdeserver") {
      groupId = "qwde.frontend"
      description = "Haskell binary to run isomorhpic HTTP frontend"
      artifact(artifact)
      artifactId = "frontend"
      pom {
        name.set("frontend")
        description.set("Haskell binary to run isomorhpic HTTP frontend + Haskell-to-Javascript code that lets you run an isomorphic website")
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
      url = uri("https://qwde.no/archiva/repository/internal/")
      credentials {
          // Store in ~/.gradle/gradle.properties
          username = "admin"
          password = repoPassword
      }
    }
  }
}

