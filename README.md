# Uploader API Demo

## Introduction

Write a program that provides an HTTP API to store and retrieve files. It should support the following features:

- Upload a new file
- Retrieve an uploaded file by name
- Delete an uploaded file by name
- Include a NixOS module to provide the API as a service
- If multiple files have similar contents, reuse the contents somehow to save space.

## Requirement
- GHC 8.2.2
- Nix
- SQLite (for store file metadata)

## How to use

Run nix-build then run execute file

```
nix-buid ./release.nix
./result/bin/file-api
```

Server will run at `http://localhost:3000`. If you want to use another port, set `PORT` environment
