FROM qwdefrontend:stage
ADD qwdeclient/ /qwde/qwdeclient
ADD qwdeserver/ /qwde/qwdeserver/ 
ADD qwdeshared/ /qwde/qwdeshared
ADD qwdeutil /qwde/qwdeutil/
ADD nix/ /qwde/nix/
ADD LICENSE README.md /qwde/
WORKDIR /qwde/
RUN nix-build
