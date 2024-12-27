import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {catchError, Observable, of} from 'rxjs';
import json from '../../appsettings.json';

@Injectable({
  providedIn: 'root'
})
export class SurgeryRoomService {

  private apiUrl = json.backendApi["1"].url + '/surgeryRoom';

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

  getRoomInfo(): Observable<{ title: string, body: string | number }[][]> {
    return this.http.get<{ title: string, body: string | number }[][]>(`${this.apiUrl}/info`, {
      withCredentials: true
    }).pipe(
      catchError(error => {
        console.error('Error getting room info:', error);
        return of([]);
      })
    );
  }
}
