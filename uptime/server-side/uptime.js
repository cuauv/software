/*
-- Uptime Ajax Script --
periodically polls /uptime/uptime.xml w/ ajax and
displays uptime in the uptime-field div.
/uptime/uptime.xml should be updated through use of
an auv-side uptime daemon.

Script currently deployed to /srv/www/http/uptime/uptime.js
Requires jQuery.

Jeff Heidel 2013
*/
$(function(){
	setInterval("do_ajax()", 500);
    $.ajaxSetup({
        cache: false
    });
	do_ajax();
});
function do_ajax(){
	$.ajax({
		type: "GET",
		url: "/uptime/uptime.xml",
		dataType: "xml",
		success: function(xml) {
			$('#uptime-field').empty();
			$(xml).find('uptime').each(function(){
				var hrs = $(this).find('hours').text();
				var mins = $(this).find('minutes').text();
				var secs = $(this).find('seconds').text();
				var start = $(this).find('start').text();
				var lastupdate = $(this).find('update').text();
				$('<div></div>').html('<big><b>'+hrs+' hour'+(hrs!=1?'s':'')+'<br>'+mins+' minute'+(mins!=1?'s':'')+'<br>'+secs+' second'+(secs!=1?'s':'')+'</b></big><br><br>Since: <b>'+start+'</b><br><br><small>Last update:<br>'+lastupdate+'</small>').appendTo('#uptime-field');
			});
		}
	});
}

