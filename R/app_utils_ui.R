useStationSelector <- function(id, label) {
  selectizeInput(
    id,
    label,
    choices = NULL,
    options = list(
      valueField = 'id',
      labelField = 'name',
      searchField = 'name',
      load = I(
        "
function(query, callback) {
  if (!query.length) return callback();

  fetch('http://transport.opendata.ch/v1/locations?query='+encodeURIComponent(query)+'&limit=10')
    .then(function(response) {
      return response.json();
  })
    .then(function(responseJSON) {
      callback(responseJSON.stations.map(station => ({
        id: station.id,
        name: station.name
      })));
  });
}"
      )
    ),
  )
}
