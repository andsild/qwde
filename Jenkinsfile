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

  stage('Publish backend') {
    dir("${env.WORKSPACE}/backend") {
      withGradle {
        sh './gradlew publish'
      }
      sh '''
        set +x # don't expose password
        token="$(cat /run/secrets/deploy-password)"
        echo "Doing curl http://qwde.no:9000/hooks/qwde-deploy?token=..."
        curl "http://qwde.no:9000/hooks/qwde-deploy?token=$token"
        set -x
        '''
    }
  }
}
