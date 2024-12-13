import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {catchError, Observable, of} from 'rxjs';
import json from '../../appsettings.json';

@Injectable({
  providedIn: 'root'
})
export class SurgeryRoomService {

  private apiUrl = json.apiUrl + '/surgeryRoom';
  private SwiUrl = "http://localhost:8080";
  private algavUrl = json.apiUrl + '/algav';



  constructor(private http: HttpClient) {
  }

  getSurgeryRooms(): Observable<any[]> {
    return this.http.get<any[]>(`${this.apiUrl}/status`, {
      withCredentials: true
    }).pipe(
      catchError(error => {
        console.error('Error getting surgery rooms:', error);
        return of([]);
      })
    );
  }
  getSurgeryPlan(): Observable<any[]> {
    return this.http.get<any[]>(`${this.SwiUrl}/ola2`).pipe(
      catchError(error => {
        console.error('Error getting surgery plan:', error);
        return of([]);
      })
    );
  }

  loadData(): Observable<any[]> {
    return this.http.get<any[]>(`${this.SwiUrl}/load`).pipe(
      catchError(error => {
        console.error('Error getting surgery plan:', error);
        return of([]);
      })
    );
  }


  getOperationTypes(): Observable<any> {
    return this.http.get(`${this.algavUrl}/operation-type`);
  }
  getRequests(): Observable<any> {
    return this.http.get(`${this.algavUrl}/operation-request`);
  }

   getSurgeryRoomsInfo(): Observable<any> {
    return this.http.get(`${this.algavUrl}/surgery-room`);
  }

  getStaff(): Observable<any> {
    return this.http.get(`${this.algavUrl}/staff`);
  }
  getRequiredStaff(): Observable<any> {
    return this.http.get(`${this.algavUrl}/RequiredStaff`);
  }
  getStaffAgenda(): Observable<any> {
    return this.http.get(`${this.algavUrl}/staff-agenda`);
  }
  getRoomAgenda(): Observable<any> {
    return this.http.get(`${this.algavUrl}/room-agenda`);
  }
}
