import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import json from "../../appsettings.json"

@Injectable({
  providedIn: 'root'
})

export class MedicalRecordService {

  private backend1ApiUrl = json.backendApi["1"].url;
  private backend2ApiUrl = json.backendApi["2"].url;


  constructor(private http: HttpClient) {
  }

  createMedicalRecord(medicalRecordId: string) {
    return this.http.post<void>(this.backend2ApiUrl + '/medicalRecord', {
      medicalRecordId: medicalRecordId
    }, {
      withCredentials: true
    });
  }

}
