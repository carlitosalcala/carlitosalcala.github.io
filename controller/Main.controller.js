sap.ui.define([
	"sap/ui/core/mvc/Controller"
], function(Controller) {
	"use strict";

	return Controller.extend("com.demoOpenWeatherSample.controller.Main", {

		onInit: function() {
			this._loadForecast();
		},
		
		rowSelectionChanged: function (evt) {
			sap.m.MessageToast.show(evt.getSource().getId() + " Pressed");
		},
		
		onClick: function(evt){
			alert(":)");
			sap.m.MessageToast.show("funny once you understand sort of js");
		},

		_formatDate: function(date) {
			var d = new Date(date),
				month = '' + (d.getMonth() + 1),
				day = '' + d.getDate(),
				year = d.getFullYear();

			if (month.length < 2) {
				month = '0' + month;
			}
			if (day.length < 2){
				day = '0' + day;	
			} 
			return [year, month, day].join('-');
		},

		_mapResults: function(results) {
			var oModel = this.getView().getModel();
			oModel.setProperty("/city", results.city.name);
			oModel.setProperty("/country", results.city.country);

			var aForecastResults = [];
			for (var i = 0; i < results.list.length; i++) {
				var oTemp = results.list[i].temp;
				var date = this._formatDate(results.list[i].dt * 1000);
				aForecastResults.push({
					date: date,
					temp: oTemp.day,
					temp_min: oTemp.min,
					temp_max: oTemp.max,
					temp_night: oTemp.night,
					temp_eve: oTemp.eve,
					temp_morn: oTemp.morn,
					units: "Celsius",
					humidity: results.list[i].humidity
				});
			}

			oModel.setProperty("/items", aForecastResults);
		},

		_loadForecast: function() {
			var oView = this.getView();
			var oParams = {
				q: "Madrid",  // Get the weather in london
				units: "metric", 
				appid: "b6131c13d379bb5429a1437e1e823848",  // replace with your API key
				cnt: 16,  // get weather for the next 16 days
				mode: "json"  // get it in JSON format 
			};
		//	var sUrl = "/OpenWeather/data/2.5/forecast/daily"; https://cors-anywhere.herokuapp.com/
			var sUrl = "https://cors-anywhere.herokuapp.com/http://api.openweathermap.org/data/2.5/forecast/daily";
			// https://www.reddit.com/r/FreeCodeCamp/comments/4ioycb/how_to_solve_mixed_content_issue/
			oView.setBusy(true);
			
			var self = this;

			$.get(sUrl, oParams)
				.done(function(results) {
					oView.setBusy(false);
					self._mapResults(results);
				})
				.fail(function(err) {
					oView.setBusy(false);
					if (err !== undefined) {
						var oErrorResponse = $.parseJSON(err.responseText);
						sap.m.MessageToast.show(oErrorResponse.message, {
							duration: 6000
						});
					} else {
						sap.m.MessageToast.show("Unknown error!");
					}
				});
}
	});
});
