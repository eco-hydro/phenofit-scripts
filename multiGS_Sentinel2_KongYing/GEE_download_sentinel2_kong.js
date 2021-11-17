/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var point = /* color: #d63000 */ee.Geometry.Point([115.24520117066619, 33.30474527061724]),
    region = 
    /* color: #98ff00 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[115.23884657570709, 33.3116168370602],
          [115.23884657570709, 33.30071344384306],
          [115.24931791970123, 33.30071344384306],
          [115.24931791970123, 33.3116168370602]]], null, false),
    imageCollection = ee.ImageCollection("COPERNICUS/S2_SR");
/***** End of imports. If edited, may not auto-convert in the playground. *****/
var pkgs = require('users/kongdd/pkgs:pkgs.js');
var range = [115.2388,33.3007, 115.2493,33.3116];
var region = ee.Geometry.Rectangle(range);
print(region);

/**
 * Function to mask clouds using the Sentinel-2 QA band
 * @param {ee.Image} image Sentinel-2 image
 * @return {ee.Image} cloud masked Sentinel-2 image
 */
function maskCloud_s2(img) {
  var qa = img.select('QA60');
  // Bits 10 and 11 are clouds and cirrus, respectively.
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;
  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
      .and(qa.bitwiseAnd(cirrusBitMask).eq(0));
  var id = ee.String(img.get('system:id'))
      .replace("COPERNICUS/S2_SR/", "");
      // .replace("_T50SLB", "");
  var ans = img.updateMask(mask)//.divide(10000)
      .copyProperties(img, img.propertyNames())
      .set('system:id', id);
  return ee.Image(ans);
}

var col = ee.ImageCollection('COPERNICUS/S2_SR')
  .filterDate('2013-01-01', '2021-12-31')
  .filterBounds(point)
  // Pre-filter to get less cloudy granules.
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE',20))
  .map(pkgs.rename_s2)
  .map(maskCloud_s2)
  .map(pkgs.mutate(['EVI2', 'EVI', 'NDVI'], {include_origin: true}));
col = pkgs.mosaicByTime(col);
print(col.size(), col);

// Map.setCenter(83.277, 17.7009, 12);
Map.centerObject(region, 14);

var vis = {min: 0.0, max: 0.3, bands: ['red', 'green', 'blue']};
Map.addLayer(col, {}, 'col');
Map.addLayer(col.mean(), vis, 'RGB');

var options = {
    type: 'drive', 
    scale: 10, // meter
    crs: 'EPSG:4326', 
    range: range,
    folder: 'rgee'
};

var bands = ["EVI", "EVI2", "NDVI", 'blue', 'green', 'red', 'nir','SCL', 'QA60'];
// pkgs.ExportImgCol(col.select(bands).limit(3), "s2_KongYing_", options);
// pkgs.ExportImgCol(col.select(bands).limit(3), "s2_KongYing_", options);
pkgs.ExportImgCol_multiBands(col.select(bands), "sentinel2_KongYing_2019-2021", options);

