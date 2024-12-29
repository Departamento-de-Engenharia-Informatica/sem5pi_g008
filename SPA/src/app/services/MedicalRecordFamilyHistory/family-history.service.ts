import { Injectable } from '@angular/core';
import json from '../../appsettings.json';
import {HttpClient, HttpErrorResponse} from '@angular/common/http';
import {catchError, Observable} from 'rxjs';
import {ɵFormGroupValue, ɵTypedOrUntyped} from '@angular/forms';

@Injectable({
  providedIn: 'root'
})
export class FamilyHistoryService {
  private apiUrl = `${json.backendApi['2'].url}/familyhistory`;

  constructor(private http: HttpClient) {}


  saveFamilyHistory(value: any): Observable<string> {
    console.log('Form submitted:', value);
    return this.http.post<string>(this.apiUrl, value, { withCredentials: true });
  }
}
