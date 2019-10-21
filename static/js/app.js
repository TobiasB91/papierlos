
let createCard = (obj) => { 
  var card = document.createElement("div");
  card.classList.add("card"); 
  
  var cardImage = document.createElement("div");
  cardImage.classList.add("card-image");
  var figure = document.createElement("figure");
  figure.classList.add("image", "is-3by4");
  var img = document.createElement("img");
  let thumb = (obj.document_thumbnail);
  img.setAttribute("src", "data:image/png;base64," + thumb);
  img.setAttribute("alt", "Placeholder image");
  card.appendChild(cardImage).appendChild(figure).appendChild(img);


  var cardContent = document.createElement("div");
  cardContent.classList.add("card-content");
  var content = document.createElement("div");
  content.classList.add("content");
  var title = document.createElement("h4");
  title.classList.add("title", "is-4");
  title.innerHTML = obj.document_name;
  var desc = document.createElement("p");
  desc.innerHTML = obj.document_content.substr(0,250);
  card.appendChild(cardContent).appendChild(content).appendChild(title); 
  content.appendChild(desc);

  return card;
};


document.addEventListener("DOMContentLoaded", () => {
  fetch("/documents").then(response => {
    if (response.ok) return response.json(); else throw new Error(response.status);
  }).then(json => { 
    let columns = document.querySelector(".columns").querySelectorAll(".column");
    [0,1,2,3].forEach(i => {
      json.filter(el => json.indexOf(el) % 4 === i).forEach(el => { 
        columns[i].appendChild(createCard(el));
      });
    });
  }).catch(err => { console.log(err); });
});
