group = "qwde"
version = "0.0.1"

val tablesawVersion by extra("0.34.2")
val kafkaVersion by extra("2.3.0")

buildscript {
    repositories {
        maven {
            url = uri("https://plugins.gradle.org/m2/")
        }
        jcenter()
        gradlePluginPortal()
    }
    dependencies {
        classpath("ru.vyarus:gradle-quality-plugin:4.1.0")
    }
}

allprojects {
    group = "qwde"

    repositories {
        mavenLocal()
        mavenCentral()
    }
}

tasks.withType<JavaCompile>().configureEach {
    options.encoding = "UTF-8"
    options.compilerArgs.addAll(listOf("-Xmx4g", "--release", "13"))
    sourceCompatibility = JavaVersion.VERSION_13.toString()
    targetCompatibility = JavaVersion.VERSION_13.toString()
}

configure(subprojects.filter { it.name == "analytics"
        || it.name == "dataprovider"
        || it.name == "webapi"
        || it.name == "trading" }) {
    apply(plugin = "java")
    apply(plugin = "ru.vyarus.quality")

    dependencies {
        "implementation"("com.google.guava:guava:23.0")
        "implementation"("ch.qos.logback:logback-classic:1.3.0-alpha4")
        "implementation"("one.util:streamex:0.7.0")
        "testImplementation"("com.flextrade.jfixture:jfixture:2.7.2")
        "testImplementation"("org.junit.jupiter:junit-jupiter-api:5.6.0")
        "testRuntimeOnly"("org.junit.jupiter:junit-jupiter-engine:5.6.0")
        "testImplementation"("com.google.truth:truth:1.0.1")
    }

    tasks.named<Test>("test") {
        useJUnitPlatform()

        maxHeapSize = "4G"

        testLogging.showExceptions = true
    }

    afterEvaluate {
        tasks.named("spotbugsMain") {
            enabled = false;
        }
        //val taskRegex = Regex(""".*spotbugsMain.*""")
        //val skipTasks = tasks.filter { it.name.matches(taskRegex) }
        //skipTasks.forEach { logger.info("{}", it.name); it.enabled = false; }
    }
}

subprojects {
    version = "1.0"
}
