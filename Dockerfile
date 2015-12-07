FROM codesimple/elm:0.16

RUN apt-get install rlwrap

RUN curl https://deb.nodesource.com/node_5.x/pool/main/n/nodejs/nodejs_5.1.0-2nodesource1~jessie1_amd64.deb > node.deb \
 && dpkg -i node.deb \
 && rm node.deb

ENV APP_DIR /usr/app
WORKDIR $APP_DIR
ENTRYPOINT npm test
