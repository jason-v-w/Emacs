


<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <meta name="viewport" content="width=device-width" />
    <!--<meta name="apple-mobile-web-app-capable" content="yes" />-->
<title>Onboarding Portal</title>
<link href="ruckus_user.css" rel="stylesheet" type="text/css" />
<link media="only screen and (max-width: 420px)" rel="stylesheet" href="ruckus_user_mobile.css" type="text/css" title="no title"/>



<script language="Javascript" type="text/javascript">
//<![CDATA[
var onloads = new Array();
function bodyOnLoad() {
    for ( var i = 0 ; i < onloads.length ; i++ ) {
        onloads[i]();
    }
}

function getCookie(name) { 
    var re = new RegExp(name + "=([^;]*)");
    var match = re.exec(document.cookie);
    if (match)
        return unescape(match[1]);
    else
        return null;
}
function deleteCookie(name) {
    createCookie(name, '', -1);
}
function createCookie(name,value,days) {
	if (days) {
		var date = new Date();
		date.setTime(date.getTime()+(days*24*60*60*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";
	document.cookie = name+"="+value+expires+"; path=/";
}

function redirectToPeerZd(peerZd) {
    if (peerZd != '') {
        location.href = peerZd;
    }
}

//]]>
</script>
</head>

<body onload="bodyOnLoad()"><table id="wrapper"><tr><td valign="middle">
<div id="dialog">
<div id="dialog_header">
    <a id="logo" href="index.jsp"><img src="/uploaded/guest_logo_1" alt="" title="" width="138px" height="40px" /></a>
    <h1>Onboarding Portal</h1>
    
</div><!-- /dialog_header -->
<div id="dialog_content">




<form method="get" action="onboarding.jsp">
    <table>
        <tr class="gap"><td><input type="hidden" name="cookie" value="" />
                            <input type="hidden" name="redirecturl" value="http://melpa.org/packages/mwim-20161004.647.el" /></td></tr>
        <tr><td><input type="submit" class="custom_button" value="Guest Access" name="guest" id="guest" /></td></tr>
        <tr><td><input type="submit" class="custom_button" value="Register Device" name="prov" id="prov" /></td></tr>
    </table>
</form>

    </div><!-- /dialog_content -->
    <div id="dialog_footer"></div>

<p id="powered"><a href="http://www.ruckuswireless.com/">Powered by Ruckus Wireless</a></p>

<iframe id="IF" name="IF" src="/_.html"></iframe>
</div><!-- /dialog -->
</td></tr></table>
</body>
</html>


