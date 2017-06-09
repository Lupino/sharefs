/**
 * @api {get} /file/:fileName Get a file.
 * @apiName GetFile
 * @apiGroup File
 *
 * @apiExample {curl} Example usage:
 *    curl http://share-fs-host/file/some/path/to/file.ext
 */
function getFile() {}

/**
 * @api {put} /file/:fileName Upload a file.
 * @apiName PutFile
 * @apiGroup File
 *
 * @apiExample {curl} Example usage:
 *    curl http://share-fs-host/file/some/path/to/file.ext \
 *      -F @file.ext \
 *      -XPUT
 */
function putFile() {}

/**
 * @api {delete} /file/:fileName Delete file or directory.
 * @apiName DeleteFile
 * @apiGroup File
 *
 * @apiExample {curl} Example usage:
 *    curl http://share-fs-host/file/some/path/to/file.ext -XDELETE
 *
 */
function deleteFile() {}

/**
 * @api {get} /stat/:fileName Stat a file or directory.
 * @apiName StatFile
 * @apiGroup File
 *
 * @apiExample {curl} Example usage:
 *    curl http://share-fs-host/stat/some/path
 *
 * @apiSuccess {String=D,F,L,E} type Type of file or directory.
 * @apiSuccess {String} name Name of file or directory.
 * @apiSuccess {Number} size Size of file or directory.
 * @apiSuccess {Number} mode Mode of file or directory.
 * @apiSuccess {Number} mtime ModificationTime of file or directory.
 * @apiSuccess {Number} ctime StatusChangeTime of file or directory.
 */
function statFile() {}

/**
 * @api {get} /dir/:fileName Get directory info.
 * @apiName GetDir
 * @apiGroup Directory
 *
 * @apiExample {curl} Example usage:
 *    curl http://share-fs-host/dir/some/path
 *
 */
function getDir() {}

/**
 * @api {put} /dir/:fileName Create directory.
 * @apiName PutDir
 * @apiGroup Directory
 *
 * @apiExample {curl} Example usage:
 *    curl http://share-fs-host/dir/some/path -XPUT
 *
 */
function putDir() {}

/**
 * @api {post} /rename/:fileName Rename a file or directory.
 * @apiName RenameFile
 * @apiGroup File
 *
 * @apiParam {String} dst Target file or directory
 *
 * @apiExample {curl} Example usage:
 *    curl http://share-fs-host/dir/some/path -d dst dst
 *
 */
function renameFile() {}
