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

}
