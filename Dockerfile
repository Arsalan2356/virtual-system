FROM rust:1.87.0

WORKDIR /usr/src/app

COPY Cargo.toml Cargo.lock ./

RUN mkdir src && echo "fn main() {}" > src/main.rs && cargo build --release

RUN rm -f src/main.rs

COPY . .

CMD ["cargo", "run", "--release"]
