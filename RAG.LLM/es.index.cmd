curl -X DELETE "localhost:9200/csv_files_index"

curl -X PUT "localhost:9200/csv_files_index" -H 'Content-Type: application/json' -d'
{
  "mappings": {
    "properties": {
      "text": {
        "type": "text",
        "analyzer": "ik_max_word",
        "search_analyzer": "ik_smart"
      },
      "active_section": {
        "type": "keyword"
      },
      "journal": {
        "type": "keyword"
      },
      "file_name": {
        "type": "keyword"
      }
    }
  }
}
'