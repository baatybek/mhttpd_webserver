function proccessClearAction() {
    let xhr = new XMLHttpRequest();
    xhr.open("GET", '/clear', true);
    xhr.send();
    xhr.onreadystatechange = function() { // Call a function when the state changes.
        if (xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200) {
            console.log( xhr.response );
            processRefreshAction();
        }
    };
}
function processRefreshAction() {
    const xhr = new XMLHttpRequest();
    xhr.open( 'GET', '/data',true );
    xhr.responseType = 'json';
    xhr.send ();
    xhr.onerror = function() {
        alert('Request failed');
    };


    xhr.onreadystatechange = function() {
        if (xhr.readyState === 4 && xhr.status === 200) {
            const data = xhr.response;
            const table = document.getElementById('dataTable');
            const newTable = document.createElement('TABLE');
            if( data ) {
                console.log(data);
                for(let i = 0; i < data.length; i++ ) {
                    let tr = document.createElement('TR');
                    let tdUsername = document.createElement('TD');
                    let tdDate = document.createElement('TD');
                    let tdMessage = document.createElement('TD');
                    let username = document.createTextNode(data[i].username);
                    let date = document.createTextNode(data[i].date);
                    let message = document.createTextNode(data[i].message);
                    tdUsername.appendChild(username);
                    tdDate.appendChild(date);
                    tdMessage.appendChild(message);
                    tr.appendChild(tdUsername);
                    tr.appendChild(tdDate);
                    tr.appendChild(tdMessage);
                    newTable.appendChild(tr);
                }
                table.innerHTML = newTable.innerHTML;
            } else {
                table.innerHTML = newTable.innerHTML;
                console.log('No content!');
            }
        }
    };
}


function processSendAction() {
    if (!validate()) {
        return;
    }
    let usernameInput = document.getElementById('username').value;
    let messageInput = document.getElementById('message').value;
    let xhr = new XMLHttpRequest();
    xhr.open("GET", '/username=' + usernameInput + '/message='+ messageInput , true);
    xhr.setRequestHeader("Content-Type", "text/plain; charset=utf-8");
    xhr.send();
    xhr.onreadystatechange = function() { // Call a function when the state changes.
        if (xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200) {
            console.log( xhr.response );
        }
    };
}

function validate() {
    const username = document.getElementById("username").value;
    const message = document.getElementById("message").value;
    if( username == null || username === "" || message == null || message === "" ) {
        alert("Both of the fields are required to fill");
        return false;
    }
    if( username.length > 16 ) {
        alert("Username length exceeds 16 characters! Username length = " + username.length );
        return false;
    }
    if( message.length > 140 ) {
        alert("Message length exceeds 140 characters! Message length = " + message.length );
        return false;
    }
    return true;
}