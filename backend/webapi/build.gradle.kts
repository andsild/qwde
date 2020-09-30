import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
  `java-library`
    application
    id("com.github.johnrengelman.shadow") version "5.2.0"
    `maven-publish`
}

application {
    mainClassName = "qwde.backend.webapi.App"
    applicationDefaultJvmArgs = listOf("-Dmicronaut.openapi.views.spec=redoc.enabled=true,rapidoc.enabled=true,swagger-ui.enabled=true,swagger-ui.theme=flattop")
}

version = "0.0.1"
description = """\
Qwde backend, reading stock data and presenting various functions through a REST-api.
"""
val micronautVersion by extra("2.0.2")
val swaggerNaut by extra("1.5.3")
val jacksonVersion by extra("2.11.2")

// Tablesaw and micronaut competes for a jackson version, apparently
configurations.all {
    resolutionStrategy {
        force("com.fasterxml.jackson.core:jackson-annotations:$jacksonVersion")
        force("com.fasterxml.jackson.core:jackson-databind:$jacksonVersion")
        force("com.fasterxml.jackson.dataformat:jackson-dataformat-yaml:$jacksonVersion")
        force("com.fasterxml.jackson.datatype:jackson-datatype-jdk8:$jacksonVersion")
        force("com.fasterxml.jackson.datatype:jackson-datatype-jsr310:$jacksonVersion")
    }
}

dependencies {
    implementation("commons-io:commons-io:2.6")
    implementation("io.prometheus:simpleclient_httpserver:0.6.0")
    implementation("io.prometheus:simpleclient_servlet:0.0.11")
    implementation("org.freemarker:freemarker:2.3.29")
    implementation("org.apache.commons:commons-lang3:3.9")
    implementation("tech.tablesaw:tablesaw-core:${project.rootProject.ext["tablesawVersion"]}")
    implementation("tech.tablesaw:tablesaw-html:${project.rootProject.ext["tablesawVersion"]}")
    implementation("tech.tablesaw:tablesaw-jsplot:${project.rootProject.ext["tablesawVersion"]}")
    api(project(":dataprovider"))
    api(project(":analytics"))
    api(project(":trading"))

    annotationProcessor(platform("io.micronaut:micronaut-bom:$micronautVersion"))
    annotationProcessor("io.micronaut:micronaut-inject-java")
    annotationProcessor("io.micronaut:micronaut-validation")
    annotationProcessor("io.micronaut.configuration:micronaut-openapi:$swaggerNaut")
    compileOnly("io.swagger.core.v3:swagger-annotations")
    implementation(platform("io.micronaut:micronaut-bom:$micronautVersion"))
    implementation("io.micronaut:micronaut-inject:$micronautVersion")
    implementation("io.micronaut:micronaut-validation:$micronautVersion")
    implementation("io.micronaut:micronaut-runtime:$micronautVersion")
    implementation("io.micronaut:micronaut-http-server-netty:$micronautVersion")
    implementation("io.micronaut:micronaut-http-client:$micronautVersion")
    implementation("io.micronaut:micronaut-management:$micronautVersion")
    implementation("info.picocli:picocli")
    implementation("io.micronaut.picocli:micronaut-picocli:2.1.1")
    //implementation("javax.annotation:javax.annotation-api:1.3.2") // https://docs.micronaut.io/latest/guide/languageSupport.html#java9

    implementation("io.micronaut.cache:micronaut-cache-caffeine:2.1.0")

    testAnnotationProcessor(enforcedPlatform("io.micronaut:micronaut-bom:$micronautVersion"))
    testAnnotationProcessor("io.micronaut:micronaut-inject-java")
    testImplementation("io.micronaut.test:micronaut-test-junit5")
}

sourceSets {
    main {
        java.srcDir("src/main/java")
    }
    test {
        java.srcDir("src/test/java")
    }
}

tasks {
  named<ShadowJar>("shadowJar") {
    archiveBaseName.set("shadow")
      mergeServiceFiles()
      manifest {
        attributes(mapOf("Main-Class" to "qwde.backend.webapi.App"))
      }
  }
}

tasks {
  build {
    dependsOn(shadowJar)
  }
}

val repoUser: String by project
val repoPassword: String by project
val completeJar = file("$buildDir/libs/shadow-$version-all.jar")
val completeArtifact = artifacts.add("archives", completeJar) {
  type = "jar"
  builtBy("shadowJar")
}

publishing {
  publications {
    create<MavenPublication>("default") {
      from(components["java"])
    }
    create<MavenPublication>("completeJar") {
      artifact(completeArtifact)
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
