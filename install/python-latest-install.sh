# Install latest version of Python (currently 3.8.0)

apt-get install -y zlib1g-dev libffi-dev
apt-get install -y libreadline-gplv2-dev libncursesw5-dev libssl-dev libsqlite3-dev tk-dev libgdbm-dev libc6-dev libbz2-dev

mkdir python-latest-install
cd python-latest-install

PYTHON_VERSION="3.8.0"
wget "https://www.python.org/ftp/python/${PYTHON_VERSION}/Python-${PYTHON_VERSION}.tgz"
tar -xvzf "Python-${PYTHON_VERSION}.tgz"

cd "Python-${PYTHON_VERSION}"

./configure
make
make altinstall

cd ..
rm -rf python-latest-install
