node {
    // Create an Artifactory Gradle instance.
    def rtGradle = Artifactory.newGradleBuild()
    def buildInfo

    stage('Clone sources') {
        git url: 'https://github.com/andsild/qwde.git'
    }
    
    stage('Clone submodule') {
      exec 'git submodule update --init --recursive'
    }

    stage('Gradle build') {
        buildInfo = rtGradle.run rootDir: ".", buildFile: 'build.gradle.kts'
    }
}
