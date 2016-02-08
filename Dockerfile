FROM node:argon

# Create app directory
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

# Install app dependencies
COPY package.json /usr/src/app/
RUN npm install && npm install -g node-gyp && \
  cd /usr/src/app/node_modules/nsqjs/node_modules/snappystream/node_modules/snappy && \
  node-gyp rebuild

# Bundle app source
COPY . /usr/src/app

EXPOSE 8000
CMD [ "npm", "start" ]
