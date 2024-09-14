#!/usr/bin/env bash

# Function to get the latest version of a HashiCorp product
get_latest_version() {
  local product=$1
  curl -s "https://releases.hashicorp.com/${product}/index.json" | jq -r '.versions | keys[]' | sort -V | tail -n 1
}

# Function to download and unzip a HashiCorp product for Linux AMD64
download_and_unzip() {
  local product=$1
  local version=$2
  local url="https://releases.hashicorp.com/${product}/${version}/${product}_${version}_linux_amd64.zip"
  # Where to put binaries
  local dest_dir="$HOME/usr/bin"

  # Create destination directory if it doesn't exist
  mkdir -p "$dest_dir"

  # Download the zip file
  curl -o "/tmp/${product}_${version}_linux_amd64.zip" "$url"

  # Unzip the binary to the destination directory
  ## Overwrite existing files
  unzip -o "/tmp/${product}_${version}_linux_amd64.zip" -d "$dest_dir"

  rm -f "/tmp/${product}_${version}_linux_amd64.zip"
  rm -f "${dest_dir}/LICENSE"

  # Make the binary executable
  chmod +x "${dest_dir}/${product}"
}

install_product() {
  local product_name=$1
  local product_version
  
  product_version=$(get_latest_version "$product_name")
  download_and_unzip "$product_name" "$product_version"

  echo "Installed $product_name, version: $product_version"
}

install_product "vagrant"
install_product "terraform"