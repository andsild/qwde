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

  stage('Env check') {
    sh "java -version"
    sh "echo $JAVA_HOME"
  }
    
  stage('Gradle build') {
    dir("${env.WORKSPACE}/backend") {
      withGradle {
        sh './gradlew build check --info'
      }
    }
  }

  stage('Gradle publish') {
    dir("${env.WORKSPACE}/backend") {
      withGradle {
        sh './gradlew publish'
      }
    }
  }
}
