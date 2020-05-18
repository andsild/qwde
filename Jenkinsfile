node {
  def buildInfo

    stage('Clone sources') {
      git url: 'https://github.com/andsild/qwde.git'
      checkout([
          $class: 'GitSCM',
          branches: scm.branches,
          doGenerateSubmoduleConfigurations: true,
          extensions: scm.extensions + [[$class: 'SubmoduleOption', parentCredentials: true]],
          userRemoteConfigs: scm.userRemoteConfigs
      ])
    }
    
    stage('Gradle build') {
        buildInfo = rtGradle.run rootDir: ".", buildFile: 'build.gradle.kts'
    }
}
