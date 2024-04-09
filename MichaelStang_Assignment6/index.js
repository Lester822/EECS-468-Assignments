// ASSIGNMENT_NAME: EECS 468 Assignment 6
// FUNCTION: A JavaScript web server that handles GET, PUT, DELETE, and MKCOL requests
// INPUTS: HTTP Requests
// OUTPUTS: Various data manipulation outcomes
// AUTHOR_NAME: Michael Stang
// COLLABORATORS: eloquentjavascript.net and in-class slides
// LINK TO REPLIT: https://replit.com/join/nkaeubqszd-mstang822
// CREATION_DATE: 03/27/24

console.log("Server Starting");  // Prints to console for debug purposes

const { createServer } = require("http"); // used for general server usage
const methods = Object.create(null); // used for general server usage
const { createReadStream } = require("fs"); // used for get
const { stat, readdir } = require("fs").promises; // used for get

const { parse } = require("url"); // used for urlPath method which is needed for all
const { resolve, sep } = require("path"); // used for urlPath
const baseDirectory = process.cwd(); // used for urlPath

const { createWriteStream } = require("fs"); // used for PUT

const { rmdir, unlink } = require("fs").promises; // Used for DELETE

const { mkdir } = require("fs").promises;  // used for MKCOL

function urlPath(url) {  
  // Function that extracts the path from the url. Gotten from JS Server slides.
  
  let { pathname } = parse(url);  // Turns a url string into a url object
  let path = resolve(decodeURIComponent(pathname).slice(1)); // Takes the URL object and extracts the path from it by slicing the result
  if (path != baseDirectory && !path.startsWith(baseDirectory + sep)) {  // Checks to see if the path requested is outside of the base directory. If it is, we need to stop it as it would be a cybersecurity concern.
    throw { status: 403, body: "Forbidden" };  // Errors with status 403 and the text forbidden
  }
  return path;  // Assuming the final path is in an okay place, it returns the path we extracted
}

function pipeStream(from, to) {
  // Function that takes things from a read stream and "pipes" it over to a write stream. Gotten from JS Server slides.
  
  return new Promise((resolve, reject) => {  // Creates a promise to handle the piping
    from.on("error", reject);  // If there is an error in reading the streeam, reject promise
    to.on("error", reject);  // If there is an error in writing the stream, reject promise
    to.on("finish", resolve);  // If there is not an error in writing the stream, resolve promise
    from.pipe(to);  // Actually does the operation of moving from read -> write.
  });
}

createServer((request, response) => {
  // Function that actually creates our server. The overall template is from JS Server slides.
  
  let handler = methods[request.method] || notAllowed;  // Basic syntax we learned to handle methods for the server
  handler(request)  // actually call the requested method or "notAllowed()"
    .catch((error) => {  // If we error on this call
      if (error.status != null) return error;  // If there is not a status return "error"
      return { body: String(error), status: 500 };  // Otherwise, if it errors with an error status, return the status and code 500. This will be returned to the requester.
    })
    .then(({ body, status = 200, type = "text/plain" }) => {  // If we don't error

      // This section actually creates the response so that methods can just return details of the response and not have to handle sending it
      response.writeHead(status, { "Content-Type": type });  // Start the response to send with its header
      if (body && body.pipe) body.pipe(response);  // If there is a body and the body contains something, move it to the response
      else response.end(body);  // Otherwise, we just end the request
    });
}).listen(8000);  // Opens the server on port 8000

async function notAllowed(request) { 
  // Function that is called if an invalid method is requested
  
  return {  // Creates an object to dictate response
    status: 405,  // Error code 405
    body: `Method ${request.method} not allowed.`,  // Text to go along error status
  };
}

methods.GET = async function (request) {
  // Function to handle the GET method. Base gotten from JS slides
  
  let path = urlPath(request.url);  // Uses our parser function to get the path from the URL
  let stats;  // Var that will hold the details of the file or dir given from the path in the URL
  try {  // Tries to get the stats (if the file/dir does not exist, it will error)
    stats = await stat(path);  // Actually uses "stat()" to get the details of the file/dir
  } catch (error) {  // If we error
    if (error.code != "ENOENT") throw error;  // If the error is not "ENOENT", throw it as it is not expected
    else return { status: 404, body: "File not found" };  // Otherwise, if is an error of the given file not existing, we want to return the error to the requester and not crash
  }
  if (stats.isDirectory()) {  // If the path leads to a dir
    return { body: (await readdir(path)).join("\n") };  // Return the contents of the dir to the user
  } else {  // Otherwise...
    return { body: createReadStream(path), type: "plain/text" };  // Return the contents of the file to the user in the response (actual response handled above after return)
  }
};

methods.PUT = async function (request) {
  // Function to handle the PUT method. Based on the JS slides.
  
  let path = urlPath(request.url);  // Gets the path given in the URL
  await pipeStream(request, createWriteStream(path));  // Uses pipeStream to move the request from the read stream to the write stream that will create the file
  return { status: 204 };  // Return a response with status 204 indicating success
};

methods.DELETE = async function (request) {
  // Function to handle the DELETE mthod. Based on the JS slides and the "GET" method
  
  let path = urlPath(request.url);  // Gets the path given in the URL
  let stats;  // Var that will hold the details of the file or dir given from the path in the URL
  try {
    stats = await stat(path);  // Actually uses "stat()" to get the details of the file/dir
  } catch (error) {  // If we error
    if (error.code != "ENOENT") throw error;  // If the error is not "ENOENT", throw it as it is not expected
    else return { status: 204 };  // Otherwise, if is an error of the given file not existing, we have nothing to delete and should just return a 204 success response
  }
  
  if (stats.isDirectory()) await rmdir(path);  // If the path leads to a dir, delete it
  else await unlink(path);  // If the path leads to a file, not a dir, delete it
  return { status: 204 };  // Return a 204 success response
};

methods.MKCOL = async function (request) {
  // Function to handle the MKCOL method.
  
  let path = urlPath(request.url);  // Gets the path from the URL as we did before
  let stats;  // Var that will hold the details of the path, it's not needed but is used for detecting duplicate folders
  try {  // Uses a try to check if the file already exists
    stats = await stat(path);  // Tries to get the details of the file
    return { status: 405 };  // If it is successful, the dir already exists and we should return an error when trying to make a dir with the same name. I know this is janky, but it works :)
  } catch (error) {  // If it doesn't exist
    mkdir(path); // Create it
    return { status: 204 };  // Return empty success response
  }
};