import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {catchError, Observable, of} from 'rxjs';
import json from '../../appsettings.json';

@Injectable({
  providedIn: 'root'
})
export class SurgeryRoomService {

  private apiUrl = json.apiUrl + '/surgeryRoom';

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
    console.error("getRoomInfo() is not implemented yet. Returning dummy data.");
    console.error("Please implement getRoomInfo() in the SPA in the Surgery Room Service");
    return of([
      // Room 1 additional data
      [
        {title: 'Parameter 1A', body: 'Value 1A'},
        {title: 'Parameter 1B', body: 'Value 1B'}
      ],
      // Room 2 additional data
      [
        {title: 'Parameter 2A', body: 'Value 2A'},
        {title: 'Parameter 2B', body: 'Value 2B'}
      ],
      // Room 3 additional data
      [
        {title: 'Parameter 3A', body: 'Value 3A'},
        {title: 'Parameter 3B', body: 'Value 3B'}
      ],
      // Room 4 additional data
      [
        {title: 'Parameter 4A', body: 'Value 4A'},
        {title: 'Parameter 4B', body: 'Value 4B'}
      ],
      // Room 5 additional data
      [
        {title: 'Parameter 5A', body: 'Value 5A'},
        {title: 'Parameter 5B', body: 'Value 5B'}
      ],
      // Room 6 additional data
      [
        {title: 'Parameter 6A', body: 'Value 6A'},
        {title: 'Parameter 6B', body: 'Value 6B'}
      ]
    ]);
  }
}
