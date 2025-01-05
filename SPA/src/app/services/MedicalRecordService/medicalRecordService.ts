import json from '../../appsettings.json';
import {HttpClient} from '@angular/common/http';
import {Injectable} from '@angular/core';
import {Observable} from 'rxjs';

export interface auxPatientData {
  Name: string;
  Birthdate: string;
  Gender: string;
  medicalRecordId: string;
}

@Injectable({
  providedIn: 'root'
})
export class MedicalRecordService {

  private apiUrl = json.backendApi["2"].url + '/medicalRecord';
  private patientUrl = json.backendApi["1"].url + '/patient';

  constructor(private http: HttpClient) {
  }

  public getPatientData(): Observable<auxPatientData> {
    return this.http.get<auxPatientData>(`${this.patientUrl}/dataToDownload`, {
      withCredentials: true
    });
  }

  public getMedicalRecordForDownload(medicalRecordId: string): Observable<any> {
    const url = `${this.apiUrl}/${medicalRecordId}/download`;
    return this.http.get(url, {
      withCredentials: true,
    });
  }

}
