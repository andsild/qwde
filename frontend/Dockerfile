FROM qwdefrontend:deps
ADD qwdeclient/ /qwde/qwdeclient
ADD qwdeserver/ /qwde/qwdeserver/ 
ADD qwdeshared/ /qwde/qwdeshared
ADD qwdeutil /qwde/qwdeutil/
ADD nix/ /qwde/nix/
ADD default.nix LICENSE README.md /qwde/
WORKDIR /qwde/
RUN nix-build -A qwdeserver --out-link qwdeserver-dist default.nix
RUN nix-build -A qwdeclient --out-link qwdeclient-dist default.nix
