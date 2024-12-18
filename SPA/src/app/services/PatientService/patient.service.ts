import {Injectable} from '@angular/core';
import {HttpClient, HttpHeaders} from '@angular/common/http';
import {Observable} from 'rxjs';
import json from '../../appsettings.json';

@Injectable({
  providedIn: 'root'
})
export class PatientService {

  private apiUrl =  json.backendApi["1"].url + '/patient';

  constructor(private http: HttpClient) {
  }
  deleteAccount(): Observable<any> {
    const url = `${this.apiUrl}/account/exclude`;

    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
    });

    return this.http.delete(url, {
      withCredentials: true,
      responseType: 'json',
      headers: headers
    });
  }
  getProfile(): Observable<any> {
    return this.http.get(`${this.apiUrl}/profile`, {
      withCredentials: true
    });
  }

  registerNumber(number: number): Observable<any> {
    const url = `${this.apiUrl}/register`;

    return this.http.post(url, null, {
      responseType: 'arraybuffer',
      withCredentials: true,
      params: {number: number}
    });
  }

  checkUsersToDelete(): Observable<any> {
    const url = `${this.apiUrl}/checkUserToDelete`;

    return this.http.get(url, {
      withCredentials: true,
      responseType: 'json'
    });
  }


  updateProfile(profileData: {
    firstName: null;
    lastName: null;
    allergiesAndMedicalConditions: any;
    phoneNumber: null;
    gender: null;
    appointmentHistory: any;
    emergencyContact: null;
    birthDate: null;
    email: null
  }): Observable<any> {
    return this.http.patch(`${this.apiUrl}/account/update`, profileData, {
        withCredentials: true
      }
    );
  }
}
