from pymongo import MongoClient
import sys

mongo_client = MongoClient(host='localhost', port=27017)  # Default port
db = mongo_client.event_data
locations = mongo_client.locations
batch = 100

# originally meant to handle applying all arcgis data to events.
# I ultimately decided it was really only efficient for the icews dataset
# for all other datasets run 7_construct_locations_multiprocess


def format_coordinate(document):
    mapname = {
        "REV_Address": "address_constructed",
        "REV_City": "city_constructed",
        "REV_CountryCode": "country_constructed",
        "REV_Postal": "postal_constructed",
        "REV_PostalExt": "postal_ext_constructed",
        "REV_Region": "region_constructed",
        "REV_Subregion": "subregion_constructed"
    }
    return {mapname[key]: document['attributes'][key] for key in
            set(mapname.keys()) & set(document['attributes'].keys())}


def format_placename(document):
    mapname = {
        "City": "city_constructed",
        "Country": "country_constructed",
        "LangCode": "language_constructed",
        "Region": "region_constructed",
        "Score": "score_constructed",
        "Subregion": "subregion_constructed",
        "Territory": "territory_constructed"
    }
    return {mapname[key]: document['attributes'][key] for key in
            set(mapname.keys()) & set(document['attributes'].keys())}


for collection in ['icews']:
    print(collection)

    count = 0

    for document in db[collection].aggregate([{"$match": {"$and": [
            {"region_constructed": {"$exists": 0}},
            {"subregion_constructed": {"$exists": 0}},
            {"city_constructed": {"$exists": 0}},
            {"address_constructed": {"$exists": 0}}
        ]}}]).batch_size(batch):

        # Show status
        count += 1
        sys.stdout.write("\r\x1b[KRecord: " + str(count))
        sys.stdout.flush()

        if 'cline' in collection:
            keys = ['GP7', 'GP8'] if collection == 'cline_speed' else ['lat', 'lon']
            docname = {key: value for key, value in zip(keys, ['Latitude', 'Longitude'])}

            identifier = {docname[key]: document[key] for key in set(keys) & set(document.keys())}
            if not identifier:
                continue

            match = list(locations.arcgis.find(identifier))
            if not match:
                continue

            constructed = format_coordinate(match[0])

            if not constructed:
                continue
            db[collection].update_one(
                {'_id': document['_id']},
                {'$set': constructed})

        else:
            if collection == 'icews':
                docname = {'Country': 'Country', 'District': 'Region', 'Province': 'Subregion', 'City': 'City'}
            elif 'acled' in collection:
                docname = {'COUNTRY': 'Country', 'ADMIN2': 'Region', 'ADMIN1': 'Subregion', 'LOCATION': 'City'}

            identifier = {docname[key]: {"$exists": 0} if key not in document else document[key].lower() for key in
                          docname.keys()}
            if not identifier:
                continue

            match = list(locations.arcgis.find(identifier))
            if not match:
                continue

            constructed = format_placename(match[0])

            if not constructed:
                continue

            db[collection].update_one(
                {'_id': document['_id']},
                {'$set': constructed})