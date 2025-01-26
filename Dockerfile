FROM ubuntu:20.04

ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
ENV LD_LIBRARY_PATH=/usr/lib/jvm/java-11-openjdk-amd64/lib/server
ENV CARGO_TERM_COLOR=always

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    build-essential \
    libssl-dev \
    pkg-config \
    unzip \
    openjdk-11-jdk \
    git && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN curl https://sh.rustup.rs -sSf | bash -s -- -y
ENV PATH="/root/.cargo/bin:$PATH"

WORKDIR /app

COPY . .

RUN git submodule update --init --recursive

RUN cargo fetch

# Download and extract ViperTools
RUN mkdir -p /app/ViperTools && \
    curl -L -o ViperToolsLinux.zip "https://github.com/viperproject/viper-ide/releases/download/$(cat viper-toolchain)/ViperToolsLinux.zip" && \
    unzip -o ViperToolsLinux.zip -d /app/ViperTools && \
    rm ViperToolsLinux.zip
ENV VIPER_HOME=/app/ViperTools/backends
ENV Z3_EXE=/app/ViperTools/z3/bin/z3

# Download and extract CakeML
RUN curl -L -o cake-x64-64.tar.gz "https://cakeml.org/regression/artefacts/$(cat cake-toolchain)/cake-x64-64.tar.gz" && \
    tar -xvzf cake-x64-64.tar.gz -C /app && \
    rm cake-x64-64.tar.gz
ENV CAKE_ML=/app/cake-x64-64/cake

# Build CakeML compiler
RUN make -C /app/cake-x64-64

CMD ["bash"]

