$(function () {

  var flash = function (type, handler) {
    var flash_type ='_flash-'+type;
    if($.cookie(flash_type)) {
      var flash = $.cookie(flash_type).split(',');
      var oid = flash[0]; // get unique message id
      var msg = flash[1]; // get actual message
      if(window.sessionStorage.getItem(oid) == null ) {
        window.sessionStorage.setItem(oid, '1');
        // delete the cookie:
        $.cookie(flash_type, null);
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
