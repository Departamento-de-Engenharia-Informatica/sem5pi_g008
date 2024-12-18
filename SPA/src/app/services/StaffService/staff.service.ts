import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { Staff } from '../../Domain/Staff';
import {CreateStaff} from '../../Domain/CreateStaff';
import json from '../../appsettings.json';

@Injectable({
  providedIn: 'root'
})
export class StaffService {
  private apiUrl = json.backendApi["1"].url + '/Staff';

  constructor(private http: HttpClient) {}

  public listAllStaffProfiles(): Observable<Staff[]> {

    return this.http.get<Staff[]>(`${this.apiUrl}`, { withCredentials: true });
  }

  public filterStaffProfilesByName(name: string): Observable<Staff[]> {

    return this.http.get<Staff[]>(`${this.apiUrl}/by-name/${name}`, { withCredentials: true });
  }

  public filterStaffProfilesByEmail(email: string): Observable<any> {

    return this.http.get<Staff[]>(`${this.apiUrl}/by-email/${email}`, { withCredentials: true });
  }

  public filterStaffProfilesBySpecialization(specialization: string): Observable<Staff[]> {

    return this.http.get<Staff[]>(`${this.apiUrl}/by-specialization/${specialization}`, { withCredentials: true });
  }

  public deleteStaffProfile(staffId: string): Observable<any> {

    return this.http.delete(`${this.apiUrl}/${staffId}`, { withCredentials: true });
  }

  public updateStaffProfile(updatedStaffProfile: Staff): Observable<any> {

    return this.http.patch(`${this.apiUrl}`, updatedStaffProfile, { withCredentials: true });
  }

  public createStaffProfile(newStaffProfile: CreateStaff): Observable<any> {

    return this.http.post(`${this.apiUrl}`, newStaffProfile, { withCredentials: true });
  }

}
