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
          sh '''
            rm -rv target || true
            mkdir target
            docker build -t qwdefrontend:jenkins .
            id=$(docker create qwdefrontend:jenkins)
            docker cp $id:/qwde/result-5/bin/qwdeserver target/qwdeserver.bin
            docker cp $id:/qwde/result-4/bin/qwdeclient.jsexe/all.js target/all.js
            docker rm -v $id
            cd target/
            tar -czvf qwdefrontend.tar.gz all.js qwdeserver.bin
          '''
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
            echo "Doing curl https://qwde.no:9000/hooks/qwde-deploy?token=..."
            curl "https://qwde.no:9000/hooks/qwde-deploy?token=$token&service=qwdefrontend-download"
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
            echo "Doing curl https://qwde.no:9000/hooks/qwde-deploy?token=..."
            curl "https://qwde.no:9000/hooks/qwde-deploy?token=$token&service=qwdebackend-download"
            set -x
            '''
        }
      }
    }
  }
}
