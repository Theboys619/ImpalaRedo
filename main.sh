if ! [ -x "$(command -v curl-config)" ]; then
  make download
else
  make build
fi