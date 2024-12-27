import { Injectable } from '@angular/core';
import json from '../../appsettings.json';
import {HttpClient, HttpErrorResponse} from '@angular/common/http';
import {catchError} from 'rxjs';
import {ɵFormGroupValue, ɵTypedOrUntyped} from '@angular/forms';

@Injectable({
  providedIn: 'root'
})
export class FamilyHistoryService {
  private apiUrl = `${json.backendApi['2'].url}/medicalConditions`;

  constructor(private http: HttpClient) {}


  saveFamilyHistory(value: ɵTypedOrUntyped<any, ɵFormGroupValue<any>, any>) {
    return this.http.post<string>(this.apiUrl, value, { withCredentials: true })
  }
}
