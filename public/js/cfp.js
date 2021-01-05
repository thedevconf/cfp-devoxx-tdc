function onSignIn(googleUser) {
  if (! googleUser){
    console.log("No google user in context.")
    return;
  }
  var id_token = googleUser.getAuthResponse().id_token;
  console.log('ID Token: ' + id_token);
  

  var profile = googleUser.getBasicProfile();
  if (profile) {
    console.log('Client ID: ' + profile.getId());
    console.log('Client Name: ' + profile.getName());
    console.log('Client Image URL: ' + profile.getImageUrl());
    console.log('Client Email: ' + profile.getEmail());
  }
  
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/gtokensignin');
  xhr.setRequestHeader('Content-Type', 'text/plain');
  xhr.onload = function() {
    var status = xhr.status;
    if(status == 200){
      console.log('Google sign in success ');
      window.location = "/cfp/home"
    }else {
      console.log("Google sign in failed");
    }
  };
  xhr.send(id_token);
}

function signOut() {
  var auth2 = gapi.auth2.getAuthInstance();
  auth2.signOut().then(function () {
    console.log('User signed out...');
  });
}
  
$( document ).ready(function() {
  console.log( "Loading Google Sign-In" );
  var googleUser = {};
  var client_id_content = $("meta[name=google-signin-client_id]").attr("content");
  
  var startApp = function() {
    gapi.load('auth2', function(){
      console.log("Loading google auth as client id "+ client_id_content);
      var auth2 = gapi.auth2.init({
        client_id: client_id_content,
        cookiepolicy: 'single_host_origin',
        prompt: 'select_account'
      });
      
      function attachSignin(element) {
        console.log("Attaching Sign In Element "+element.id);
        auth2.attachClickHandler(element, {},
          onSignIn, 
          function(error) {
            console.log(JSON.stringify(error, undefined, 2));
          });
        }
      
      var sign_in_el = document.getElementById("btn-google-signin");
      console.log("Attaching custom sign-ing element "+sign_in_el);
      if(sign_in_el)
        attachSignin(sign_in_el);
      
    });
  };
  
  startApp();
});



