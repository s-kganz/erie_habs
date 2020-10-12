/**
 * Function to mask clouds using the Sentinel-2 QA band
 * @param {ee.Image} image Sentinel-2 image
 * @return {ee.Image} cloud masked Sentinel-2 image
 */
var maskS2clouds = function maskS2clouds(image) {
  var qa = image.select('QA60');

  // Bits 10 and 11 are clouds and cirrus, respectively.
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;

  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
      .and(qa.bitwiseAnd(cirrusBitMask).eq(0));

  return image.updateMask(mask)
              .divide(10000)
              .copyProperties(image, ['system:time_start']);
};

// Continuous monitoring points at Lake Erie
var points = ee.FeatureCollection([
  ee.Feature(ee.Geometry.Point([-83.193, 41.827]), {name: 'WE4'}),
  ee.Feature(ee.Geometry.Point([-83.330, 41.762]), {name: 'WE2'}),
  ee.Feature(ee.Geometry.Point([-83.364, 41.834]), {name: 'WE8'}),
  ee.Feature(ee.Geometry.Point([-83.385, 41.705]), {name: 'WE6'}),
  ee.Feature(ee.Geometry.Point([-83.254, 41.703]), {name: 'WE12'}),
  ee.Feature(ee.Geometry.Point([-83.424, 41.718]), {name: 'WE9'}),
  ee.Feature(ee.Geometry.Point([-83.143, 41.660]), {name: 'WE16'}),
  ee.Feature(ee.Geometry.Point([-83.136, 41.741]), {name: 'WE13'})
]);

print(points);

s2 = s2.filter(ee.Filter.calendarRange(3, 11, 'month'))
  .filterBounds(points)
  .map(maskS2clouds);
  
print(s2.size())

Map.addLayer(s2.first(), {bands: ['B4', 'B3', 'B2'], max:0.3, min: 0})
Map.addLayer(points)

var timeseries = ui.Chart.image.seriesByRegion({
  imageCollection: s2,
  regions: points,
  reducer: ee.Reducer.mean(),
  scale: 50,
  band: 'B11',
  seriesProperty: 'name'
});

print(timeseries) // open the chart in new tab, save csv, repeat for all bands...