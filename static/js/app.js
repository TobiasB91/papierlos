const GLOBAL_NUM_CARDS_PER_LOAD = 1;
var global_offset = 0;


let createCard = obj => { 

  let card = htmlToNode(`
    <div  class="card">
      <div class="card-image">
        <figure class="image is-3by3">
          <img src="data:image/png;base64,${obj.document_thumbnail}" 
            alt="PLACEHOLDER">
        </figure>
      </div>

      <div class="card-content">
        <div class="content">
          <h4 class="title is-4">${obj.document_name}</h4>
          <p>${obj.document_content.substr(0,250)}</p>
        </div>
      </div>
    </div>
  `);

  
  card.addEventListener("click", e => {
    let url = new URL(window.location.href);
    window.location.href = url.origin + "?id=" + obj.document_id;
  });

  return card;
};


document.addEventListener("DOMContentLoaded", () => {
  let url = new URL(window.location.href);
  let mayId = url.searchParams.get("id");
  let maySettings = url.searchParams.get("settings");

  if (mayId != "" && mayId != null) {
    loadDetail(mayId);
  } else if (maySettings == 1) {
    loadCreateTag();
  } else {
    loadHome();
  }
});


let loadHome = () => {
  document.querySelector("nav.breadcrumb ul li").classList.add("is-active");

  fetch("/documents").then(response => {
    if (response.ok) return response.json(); else throw new Error(response.status);
  }).then(json => { 
    createColumns(4);
    let columns = document.querySelector(".columns").querySelectorAll(".column");

    [0,1,2,3].forEach(i => {
      json.filter(el => json.indexOf(el) % 4 === i).forEach(el => { 
        columns[i].appendChild(createCard(el));
      });
    });
  }).catch(err => { console.log(err); });
};


let loadDetail = id => {
  fetch("/documents/"+id).then(response => {
    if (response.ok) return response.json(); else throw new Error(response.status);
  }).then(json => { 
    createColumns(1);
    let main = document.querySelector(".columns .column");
    
    let body = `
      <h1 class="title">${json.document_name}</h1>
      <textarea class="textarea detail-content" rows="25">
        ${json.document_content} 
      </textarea>

      <br>
      <p class="buttons">
        <a href="data:application/pdf;base64,${json.document_pdf}" 
          download="${json.document_name}" class="button is-success">
          <span>Download</span>
        </a>
      </p>
    `;
    main.innerHTML = body;

  }).catch(err => { 
    console.log(err); 
  });
};

let loadCreateTag = () => {
  createColumns(1);
  let main = document.querySelector(".columns .column");

  let body = `
    <h1 class="title">Create a tag</h1>
    <form action="/tags/create" method="post">
      <p>
        <label for="name">name:</label>
        <input type="text" name="name" id="name">
      </p>
      <p>
        <label for="matches">matches:</label>
        <input type="text" name="matches" id="matches">
      </p>
      
      <p>
        <button type="submit">Create</button>
      </p>
    </form>
  `;

  main.innerHTML = body;
  
};

let createColumns = n => { 
  document.querySelectorAll(".columns .column").forEach(el => { el.remove(); });
  [...Array(n).keys()].forEach(i => {
    let col = document.createElement("div");
    col.classList.add("column");
    document.querySelector(".columns").appendChild(col); 
  });
};


let htmlToNode = html => {
  let tmp = document.createElement("template");
  tmp.innerHTML = html.trim();
  return tmp.content.firstChild;
};
