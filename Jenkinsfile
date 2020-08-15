pipeline {
  agent any
  stages {
    stage('Clone sources') {
      steps {
        git url: 'https://github.com/andsild/qwde.git'
          checkout([
              $class: 'GitSCM',
              branches: scm.branches,
              doGenerateSubmoduleConfigurations: true,
              extensions: scm.extensions + [[$class: 'SubmoduleOption', parentCredentials: true]],
              userRemoteConfigs: scm.userRemoteConfigs
          ])
      }
    }
    stage('Env check') {
      steps {
        sh "java -version"
          sh "echo $JAVA_HOME"
      }
    }

    stage('Gradle build backend') {
      when {
        changeset "backend/**"
      }
      steps {
        dir("${env.WORKSPACE}/backend") {
          withGradle {
            sh './gradlew build check --info'
          }
        }
      }
    }
    stage('Gradle build frontend') {
      when {
        changeset "frontend/**"
      }
      steps {
        dir("${env.WORKSPACE}/frontend") {
          withGradle {
            sh './gradlew build --info'
          }
        }
      }
    }

    stage('Publish backend') {
      when {
        anyOf {
          changeset "backend/**"
        }
      }
      steps {
        dir("${env.WORKSPACE}/backend") {
          withGradle {
            sh './gradlew publish'
          }
          sh '''
            set +x # don't expose password
            token="$(cat /run/secrets/deploy-password)"
            echo "Doing curl https://qwde.no/archiva/hooks/qwde-deploy?token=..."
            curl "https://qwde.no/archiva/hooks/qwde-deploy?token=$token&service=qwdefrontend-download"
            set -x
            '''
        }
      }
    }
    
    stage ('Publish frontend') {
      when {
        anyOf {
          changeset "frontend/**"
        }
      }
      steps {
        dir("${env.WORKSPACE}/frontend") {
          withGradle {
            sh './gradlew publish --info'
          }
          sh '''
            set +x # don't expose password
            token="$(cat /run/secrets/deploy-password)"
            echo "Doing curl https://qwde.no/archiva/hooks/qwde-deploy?token=..."
            curl "https://qwde.no/archiva/hooks/qwde-deploy?token=$token&service=qwdebackend-download"
            set -x
            '''
        }
      }
    }
  }
}
