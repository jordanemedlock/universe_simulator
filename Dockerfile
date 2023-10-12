FROM fpco/alpine-haskell-stack:9.2.5

WORKDIR /opt/universe_simulator

# RUN stack install

# Add just the .cabal file to capture dependencies
COPY ./stack.yaml /opt/universe_simulator/stack.yaml
COPY ./stack.yaml.lock /opt/universe_simulator/stack.yaml.lock
COPY ./package.yaml /opt/universe_simulator/package.yaml
COPY . /opt/universe_simulator/

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack build --only-dependencies

# Add and Install Application Code
RUN stack install

CMD ["sh"]