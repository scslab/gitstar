/* This is an implementation of error, success, and info alerts
 * compatible with bootstrap.js. The current implementation relies on
 * cookies and HTML5 session storage. The former is used by the server
 * to set a _flash-* key to the message value; the latter is used to
 * keep track of which messages have been displayed. The cookie is
 * immediately delete, while the session storage item persists until
 * the window is closed. */

$(function () {

  var flash = function (type, handler) {
    var flash_type ='_flash-'+type;
    if($.cookie(flash_type)) {
      var flash = $.cookie(flash_type).slice(1,-1).split('|');
      var oid = flash[0]; // get unique message id
      var msg = flash[1]; // get actual message
      if(window.sessionStorage.getItem(oid) == null ) {
        window.sessionStorage.setItem(oid, '1');
        $.cookie(flash_type, null); // delete the cookie
        $("#flash-messages").append(
            '<div class="alert fade in alert-'+type+'">'
           +'<a class="close" data-dismiss="alert">&times;</a>'
           + handler(msg)
           + '</div>');
      }
    }
  }

  flash('error', function (msg){ return '<strong>Error:</strong>' + msg; });
  flash('success', function (msg){ return '<strong>Success:</strong>' + msg; });
  flash('info', function (msg){ return msg; });

});
