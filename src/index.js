import { Elm } from './App/Main.elm';
import registerServiceWorker from './registerServiceWorker';

const basePath = new URL(document.baseURI).pathname;

let app = Elm.App.Main.init({
  node: document.getElementById('root'),
  flags : { basePath }
});

app.ports.requestNodes.subscribe(function ( message ) {
  const axios = require('axios').default;
  let t = true
  ns = []
  axios.get('https://secret-ocean-49799.herokuapp.com/https://prices.azure.com/api/retail/prices')
    .then(function (response) {

      if (t) {
        t = false
        console.log(response.data);
      }
    })
    .catch(function (error) {
      console.log(error);
    });

   let pricing = new AWS.Pricing({
      region: message[0],
      apiVersion: '2017-10-15',
      accessKeyId: process.env.ELM_APP_ACCESS_KEY_ID,
      secretAccessKey: process.env.ELM_APP_SERCRET_ACCESS_KEY
   });
   let nextToken = message[1];
   let maxResults = message[2];
   let params = { Filters: [
         {
        Field: 'ServiceCode',
        Type: 'TERM_MATCH',
        Value: 'AmazonEC2'
      }
    ],
    ServiceCode: 'AmazonEC2',
    FormatVersion: 'aws_v1',
    MaxResults: maxResults,
    NextToken: nextToken
  };
  let s = true
  pricing.getProducts(params, function (err, data) {
    if (err) {
      console.log(err);
    } else {
      if (s) {
        s = false
        console.log(JSON.stringify(data))
      }
      // send aggregated data
      app.ports.receiveNodes.send(JSON.stringify(data));  // successful response -- DECIDE: send back string JSON or just object?
    }
  });
});

function getAzureNodes() {
  const axios = require('axios').default;
  let t = true
  axios.get('https://secret-ocean-49799.herokuapp.com/https://prices.azure.com/api/retail/prices')
    .then(function (response) {
      const body = response.data;
      return standardizeAzureNodes(body)
    })
    .catch(function (error) {
      console.log(error);
    });
}

function standardizeAzureNodes(nodes) {
  standardizedNodes = []
  for (let i = 0; i < nodes.length; i++) {
    standardizedNode = standardizeAzureNode(nodes[i])
    standardizedNodes.push(standardizedNodes)
  }
  return standardizedNodes
}

function standardizeAzureNode(node) {
  const price = {
    "unit": "Hrs",
    "endRange": "Inf",
    "description": "$0.00 per Linux c3.large Dedicated Host Instance hour",
    "appliesTo": [
      
    ],
    "rateCode": node.skuId,
    "beginRange": "0",
    "pricePerUnit": {
      "USD": node.unitPrice
    }
  }

  const priceDim = {}

  priceDim[node.skuId] = price

  const product = {
    "priceDimensions": priceDim,
    "sku": node.skuId,
    "effectiveDate": node.effectiveStartDate,
    "offerTermCode": "JRTCKXETXF",
    "termAttributes": {}
  }

  const onDemand = {}
  onDemand[node.productId] = product
  
  const terms = {
    "OnDemand": onDemand
  }
  const awsNode = {
    "product": {
      "productFamily": node.serviceFamily,
      "attributes": {
        "enhancedNetworkingSupported": "Yes",
        "intelTurboAvailable": "Yes",
        "memory": "3.75 GiB",
        "vcpu": "2",
        "classicnetworkingsupport": "true",
        "capacitystatus": "Used",
        "locationType": "AWS Region",
        "storage": "2 x 16 SSD",
        "instanceFamily": "Compute optimized",
        "operatingSystem": "Linux",
        "intelAvx2Available": "No",
        "physicalProcessor": "Intel Xeon E5-2680 v2 (Ivy Bridge)",
        "clockSpeed": "2.8 GHz",
        "ecu": "7",
        "networkPerformance": "Moderate",
        "servicename": node.productName,
        "vpcnetworkingsupport": "true",
        "instanceType": node.type,
        "tenancy": "Host",
        "usagetype": "HostBoxUsage:c3.large",
        "normalizationSizeFactor": "4",
        "intelAvxAvailable": "Yes",
        "processorFeatures": "Intel AVX; Intel Turbo",
        "servicecode": node.serviceName,
        "licenseModel": "No License required",
        "currentGeneration": "No",
        "preInstalledSw": "NA",
        "location": node.location,
        "processorArchitecture": "32-bit or 64-bit",
        "marketoption": "OnDemand",
        "operation": "RunInstances",
        "availabilityzone": "NA"
      },
      "sku": node.skuId
    },
    "serviceCode": node.serviceName,
    "terms": terms,
    "version": "20211104150900",
    "publicationDate": node.effectiveStartDate
  }

  return awsNode
}

registerServiceWorker();
