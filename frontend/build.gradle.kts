plugins {
  `maven-publish`
}

version = "0.0.1"
description = """
  Qwde frontend, reading stock data and presenting various functions through an isomorhpic HTTP site.
"""

task<Exec>("build.qwdeserver") {
  description = "build qwdeserver"
  commandLine("cabal", "install", "--program-suffix=.bin", "qwdeserver", "--installdir=dist-newstyle/target", "--overwrite-policy=always")
  inputs.files(fileTree("./qwdeserver"))
    .withPropertyName("sourceFiles")
    .withPathSensitivity(PathSensitivity.RELATIVE)
  outputs.dir("./dist-newstyle/target/")
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


task<Tar>("zipper") {
  dependsOn("build.qwdeserver")
  dependsOn("build.qwdeclient")
  compression = Compression.GZIP
  from("./dist-newstyle/") {
    includeEmptyDirs = false
    include(setOf("**/qwdeserver/build/qwdeserver/qwdeserver", "**/qwdeclient/qwdeclient.jsexe/all.js"))

    filesMatching("**/qwdeserver/build/qwdeserver/qwdeserver") {
      path = "qwdefrontend/"
      name = "qwdefrontend/qwdeserver.bin"
      }
    filesMatching("**/qwdeclient/qwdeclient.jsexe/all.js") {
      path = "qwdefrontend/"
      name = "qwdefrontend/all.js"
      }
  }
  into("qwdefrontend")
  setArchiveName("qwdefrontend.tar.gz")
  setDestinationDir(File("dist-newstyle/target/"))
}

val exec = file("dist-newstyle/target/qwdefrontend.tar.gz")
val haskell by configurations.creating
val artifact = artifacts.add("haskell", exec) {
  type = "tarball"
  builtBy("build.qwdeserver")
  classifier = "prod"
  extension = "tar.gz"
  name = "qwdeserver"
}
val repoUser: String by project
val repoPassword: String by project

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
      url = uri("http://qwde.no:8876/repository/internal/")
      credentials {
          // Store in ~/.gradle/gradle.properties
          username = "admin"
          password = repoPassword
      }
    }
  }
}

