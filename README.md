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
- NixOps (for deployment)

## Local Development

Run nix-build then run execute file

```
nix-buid ./release.nix
./result/bin/file-api
```

Server will run at `http://localhost:3000`. If you want to use another port, set `PORT` environment

## Deployment

Use NixOps to deploy
Support targets:
- NixOS machines: `release-nixos.nix`
- Virtualbox: `release-vbox.nix`
- AWS EC2: `release-ec2.nix`

Default directory for database and file storage is `/tmp`. Default port is `3000`. You can change the config in `release-configuration.nix` file


## Reuse similar content

Store file metadata in Database and content in hard disk. If file content is same, just insert a record with path uri point to absolute path in hard disk without writing file. It is same as a shortcut/link file



# API Documentation

## Upload File (can upload many files)

**URL** : `/uploads/`

**Method** : `POST`

**Content-Type**: `application/x-www-form-urlencoded`

**Auth required** : NO

### Success Response

**Code** : `200 OK`

**Content example**

```json
[{
    "name": "file.jpg",
    "type": "image/jpeg",
    "size": 300000,
    "uri": "uploads/file.jpg",
    "isLink": 0 // 0: Physic file, 1: Shortcut file
}]
```

## Direct Upload

**URL** : `/upload-direct`

**Method** : `POST`

**Content-Type**: `image/jpeg` (Depend on file's MIME type)

**Auth required** : NO

### Success Response

**Code** : `200 OK`

**Content example**

```json
[{
    "name": "file.jpg",
    "type": "image/jpeg",
    "size": 300000,
    "uri": "uploads/file.jpg",
    "isLink": 0 // 0: Physic file, 1: Shortcut file
}]
```

### Error Response

**Condition** : X-File-Name or Content-Type header is empty

**Code** : `400 BAD REQUEST`

**Content** :

```json
{
    "message": "X-File-Name is required"
}
```


## GET file by name

**URL** : `/uploads/:name`

**Method** : `GET`

**Auth required** : NO

### Success Response

**Code** : `200 OK`

**Content** : File content

### Error Response

**Condition** : File not found

**Code** : `404 NOT FOUND`

**Content** :

```json
{
    "message": "File not found"
}
```

## DELETE file by name

**URL** : `/uploads/:name`

**Method** : `DELETE`

**Auth required** : NO

### Success Response

**Code** : `204 NO CONTENT`

### Error Response

**Condition** : File not found

**Code** : `404 NOT FOUND`

**Content** :

```json
{
    "message": "File not found"
}
```
