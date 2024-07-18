function fetchCSRFToken() {
  return fetch('/get-csrf-token', {
    method: 'GET',
    credentials: 'same-origin'
  }).then(
    res => {
      if (!res.ok) {
        throw new Error('Network response was not ok');
      }
      return res.headers.get('X-Csrf-token');
    }
  ).then(token => {
    if (!token) {
      throw new Error('CSRF token not found in response headers');
    }
    return token;
  })
    .catch(error => {
      console.error('Error fetching CSRF token:', error);
      // Handle the error appropriately
    });
}

document.addEventListener('DOMContentLoaded', () => {
  fetchCSRFToken()
    .then(token => {
      // We'll store the token in the next step
      console.log('CSRF Token received:', token);
    })
    .catch(error => {
      // Handle any errors that occurred during token fetching
      console.error('Failed to fetch CSRF token:', error);
    });
});