'='
- Poverty dataset with Brown TA2

```
docker run --rm --name ta2_server\
 -e D3MRUN=ta2ta3\
 -e D3MINPUTDIR=/ravens_volume/test_data/DA_poverty_estimation\
 -e D3MPROBLEMPATH=/ravens_volume/test_data/DA_poverty_estimation/TRAIN/problem_TRAIN/problemDoc.json\
 -e D3MOUTPUTDIR=/ravens_volume/test_output/DA_poverty_estimation\
 -e D3MLOCALDIR=/ravens_volume/test_output/DA_poverty_estimation/local_dir\
 -e D3MSTATICDIR=/ravens_volume/test_output/DA_poverty_estimation/static_dir\
 -e D3MTIMEOUT=600\
 -e D3MCPU=1\
 -e D3MRAM=1048576000\
 -p 45042:45042\
 -e D3MPORT=45042\
 -v /ravens_volume/test_data/DA_poverty_estimation:/input\
 -v /ravens_volume/test_output/DA_poverty_estimation:/output\
 -v /ravens_volume:/ravens_volume\
 registry.datadrivendiscovery.org/zshang/docker_images:ta2
```

```
from tworaven_apps.ta2_interfaces.models import StoredRequest, StoredResponse

l = StoredResponse.objects.filter(stored_request__request_type='GetFitSolutionResults')

for sr in l:
  info = f"{sr.id}:  {sr.response['fittedSolutionId']}"
  print(info)


# -------------------
l = StoredResponse.objects.filter(stored_request__request_type='GetSearchSolutionsResults')
for sr in l:
  info = f"{sr.id}:  {sr.response['solutionId']}"
  print(info)

```
