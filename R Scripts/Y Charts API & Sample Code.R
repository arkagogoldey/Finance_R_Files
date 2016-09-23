# This client serves as a basic model of how to interact with our api in R. It is
# limited to our /companies endpoint and does not perform any client side validation
# of inputs. For further information regarding the capabilities of the api,
# please visit: https://ycharts.com/api/docs/
#
# Dependencies:
#   RCurl
#   rjson
#
# R -f api_v3_sample_client.r
#
#
install.packages("RCurl",repos='http://cran.rstudio.com/')
install.packages("rjson",repos='http://cran.rstudio.com/')
#

library("RCurl")
library("rjson")


setClass (Class = "YChartsApiClient",
          representation = representation(
            api_key="ANY",
            base_url="ANY",
            funct = "function"
          ),
          prototype = list ( api_key="OK/bRlCfem/K+BXruue8Ng", base_url="https://ycharts.com/api/v3/",
                             funct = function (.Object) { print (.Object) }   ),
)

setMethod ('initialize', 'YChartsApiClient',
           function(.Object, api_key='', base_url='') {
             if(nargs() > 1) .Object@api_key  = api_key
             if(nargs() > 2) .Object@base_url = base_url
             .Object
           })

# Get json response from server for the url string and params
# @param url_str the requested url
# @param @params the query string parameters
# @return JSON response of the requested url
setGeneric("fetch_data",  function(.Object, y_url, params="") standardGeneric("fetch_data"))
setMethod("fetch_data",  "YChartsApiClient",
          function(.Object, y_url, params='') {
            if ( params != "" ) y_url = paste(y_url, params, sep='', collapse='')
            headers=list('X-YCHARTSAUTHORIZATION'=.Object@api_key)
            print (headers)
            return (getURL ( y_url, httpheader = headers, ssl.verifypeer = FALSE ))
          })

# Get company infos for given companies and info_fields
# @param companies list of symbols for companies (eg. list('AAPL', 'MSFT'))
# @param info_fields list of requested info fields (eg. list('exchange'))
# @return a R class of the decoded JSON response of requested info fields for the companies
setGeneric("get_company_info", function(.Object, companies, info_fields) standardGeneric("get_company_info"))
setMethod("get_company_info", "YChartsApiClient",
          function(.Object, companies, info_fields) {
            companies = paste(companies, sep=',', collapse=',')
            info_fields = paste(info_fields, sep=',', collapse=',')
            y_url = paste (.Object@base_url, paste('companies', companies, 'info', info_fields, sep='/', collapse=NULL), sep='', collapse=NULL)
            json_data = fetch_data(.Object, y_url)
            return (fromJSON(json_data))
          })

# Get company data points for given companies and metrics
# @param companies list of symbols for companies (eg. list('AAPL', 'MSFT'))
# @param metrics list of requested info fields (eg. list('price', 'pe_ratio'))
# @param date_attr string in the YYYY-MM-DD format (if empty, it will retrieve current data point)
# @return a R class of the decoded JSON response of requested data points for the companies
setGeneric("get_company_data_point", function(.Object, companies, metrics, date_attr='') standardGeneric("get_company_data_point"))
setMethod("get_company_data_point", "YChartsApiClient",
          function(.Object, companies, metrics, date_attr='') {
            companies = paste(companies, sep=',', collapse=',')
            metrics = paste(metrics, sep=',', collapse=',')
            
            y_url = paste (.Object@base_url, paste('companies', companies, 'points', metrics, sep='/', collapse=NULL), sep='', collapse=NULL)
            params = ''
            if (date_attr != '' ) {
              params = URLencode( paste('?date=', date_attr, sep='', collapse=NULL) )
            }
            
            json_data = fetch_data(.Object, y_url, params)
            return (fromJSON(json_data))
          })

# Get company data series for given companies, metrics, start_date, and end_date
# @param companies array of symbols for companies (eg. ['AAPL', 'MSFT'])
# @param metrics array of requested metrics (eg. ['price', 'pe_ratio'])
# @param start_date string in the YYYY-MM-DD format representing start date of series
# @param end_date string in the YYYY-MM-DD format representing end date of series
# @return a R class of the decoded JSON response of requested data series for the companies
setGeneric("get_company_data_timeseries", function(.Object, companies, metrics, start_date='', end_date='') standardGeneric("get_company_data_timeseries"))
setMethod("get_company_data_timeseries", "YChartsApiClient",
          function(.Object, companies, metrics, start_date='', end_date='') {
            companies = paste(companies, sep=',', collapse=',')
            metrics = paste(metrics, sep=',', collapse=',')
            y_url = paste (.Object@base_url, 'companies', companies, 'series', metrics, sep='/', collapse=NULL)
            
            params = URLencode ( paste('?start_date=', start_date, '&', 'end_date=', end_date, sep='', collapse=NULL ))
            
            json_data = fetch_data( .Object, y_url, params )
            
            return (fromJSON( json_data ))
          })

#######
# Sample Code Using the API Client
#######
sac = new ("YChartsApiClient", api_key='OK/bRlCfem/K+BXruue8Ng') # Enter the Key here. See http://ycharts.com/accounts/my_account

test_info = get_company_info(sac, list('AAPL', 'MSFT'), list('exchange', 'industry'))
test_points = get_company_data_point(sac, list('AAPL', 'MSFT'), list('price', 'pe_ratio'), '2016-03-03')
test_series = get_company_data_timeseries(sac, list('AAPL', 'MSFT'), list('price'), '2016-3-3', '2016-3-15')

print (test_info)
print (test_points)
print (test_series)