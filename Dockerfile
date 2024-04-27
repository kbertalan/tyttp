FROM ghcr.io/stefan-hoeck/idris2-pack AS builder

RUN apt-get update
RUN apt-get install -y nodejs

WORKDIR /tyttp
COPY . .

RUN pack --no-prompt build tyttp.ipkg
RUN pack --no-prompt build json/tyttp-json.ipkg
RUN pack --no-prompt build adapter-node/tyttp-adapter-node.ipkg

RUN pack --no-prompt test tyttp
RUN pack --no-prompt test tyttp-json
RUN pack --no-prompt test tyttp-adapter-node

EXPOSE 3000
CMD pack run tyttp-adapter-node
