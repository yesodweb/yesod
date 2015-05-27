FROM haskell:7.10
MAINTAINER Greg Weber

# Intended as a development environment
#
#    docker build -t yesod .
#    docker run --rm -i -t -v `pwd`:/home/haskell yesod /bin/bash
#    stackage update
#

RUN apt-get update && apt-get install sudo \
    # ssl stuff that you may find useful
 && apt-get install -y libssl-dev ca-certificates libcurl4-openssl-dev \
    # stackage-cli uses git. authbind can be useful for exposing ports
 && apt-get install -y git authbind \
 && apt-get clean

# run as a user named "haskell"
RUN useradd -m -d /home/haskell -s /bin/bash haskell
RUN mkdir -p /etc/sudoers.d && echo "haskell ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/haskell && chmod 0440 /etc/sudoers.d/haskell
ENV HOME /home/haskell
WORKDIR /home/haskell
USER haskell
ENV LANG   C.UTF-8
ENV LC_ALL C.UTF-8

# install stackage binaries to /opt/stackage
RUN sudo mkdir -p /opt/stackage/bin
ENV PATH /opt/stackage/bin:.cabal-sandbox/bin:.cabal/bin:$PATH:./
RUN sudo chown haskell:haskell /opt/stackage/bin
RUN cabal update \
 && cabal install stackage-update && stackage-update \
 && cabal install stackage-install \
 && stackage-install stackage-cli stackage-cabal stackage-sandbox stackage-upload \
 && mv /home/haskell/.cabal/bin/* /opt/stackage/bin/ \
 && rm -r /home/haskell/.cabal 
