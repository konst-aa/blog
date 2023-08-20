## Aw.
<form id="unsubscribe-form">
        <input type="text" name="email" id="unsubscribe-email" placeholder="youremail@example.com">
        <button type="submit" value="unsubscribe" onclick="document.getElementById('unsubscribe-message').innerHTML = 'unsubscribed!'"> unsubscribe </button>
        <a id="unsubscribe-message"></a>
</form>
<script>
let unsubscribeForm = document.getElementById("unsubscribe-form");
unsubscribeForm.addEventListener("submit", (e) => {
  e.preventDefault();
  let email = document.getElementById("unsubscribe-email").value;
  var ImageObject = new Image();
  console.log("trying to unsubscribe");
  ImageObject.src = "http://ml.dreadmaw.industries/unsubscribe?email=" + email;
});
</script>
