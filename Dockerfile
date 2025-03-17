FROM ubuntu:24.04

RUN apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -y curl xz-utils wget jq zlib1g-dev libelf-dev libdw-dev libncurses5-dev bash cabal-install time python3 \
    python3-pip xdg-utils ghc 

RUN rm /usr/lib/python3.12/EXTERNALLY-MANAGED
RUN pip3 install matplotlib flask
RUN mkdir code

WORKDIR /usr/share 
RUN  wget -O agda-stdlib.tar https://github.com/agda/agda-stdlib/archive/v1.2.tar.gz
RUN tar -zxvf agda-stdlib.tar

WORKDIR /root
RUN mkdir .agda
RUN echo "/usr/share/agda-stdlib-1.2/standard-library.agda-lib" >> /root/.agda/libraries
RUN echo "standard-library" >> /root/.agda/defaults
RUN cabal update
RUN cabal install ieee754

RUN /sbin/useradd -m nixusr
RUN mkdir /nix
RUN chown nixusr /nix
USER nixusr
ENV USER=nixusr
ENV PATH="/home/nixusr/.nix-profile/bin:${PATH}"
RUN curl -sL https://nixos.org/nix/install | sh -s -- --no-daemon
RUN nix-env -iA nixpkgs.coq nixpkgs.idris2 nixpkgs.haskellPackages.Agda nixpkgs.lean4 

USER root
ENV USER=root

ENV AGDA_DIR="/root/.agda" 

WORKDIR /code
COPY mhpgeez .
COPY translator-folder/main .
COPY visualization/. .

RUN chmod +x main mhpgeez




ENTRYPOINT ["/bin/bash", "-c", "tail -f /dev/null"]