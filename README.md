# Running this code

Simply download the project and use cargo to build the repository

`git clone git@github.com:Arsalan2356/virtual-system.git`

`cargo build --release`

`./target/release/virtual-system`


If you would prefer not to build the project and simply run it you can also do

`cargo run -r .`


Note that this project was built on Linux and has NOT been tested for compatibility on Windows and Mac.

The debug build takes significantly longer to run for this project so I would recommend using the release build instead.

# Running with docker

Pull the docker image

`docker pull rc2356/virtual-system`

Run the image (This also builds it as well, so it might take slightly longer)

`docker run --rm -it -v "$(pwd)":/usr/src/app rc2356/virtual-system`

From my experience this usually takes around 7 minutes on my system (1 per config)
