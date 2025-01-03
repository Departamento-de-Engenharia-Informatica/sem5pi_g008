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


  saveFamilyHistory(value: any, medicalRecordId: string): Observable<string> {
    console.log('Form submitted:', value);
    const payload = {
      medicalRecordId: medicalRecordId,
      familyMember: value.familyMember,
      condition: value.condition,
      comment: value.comment
    };
    return this.http.post<string>(this.apiUrl, payload, { withCredentials: true });
  }

  getMedicalRecordFamilyHistoryWithIds(medicalRecordId: string) {
    const url = `${this.apiUrl}/${medicalRecordId}`;
    return this.http.get<{ medicalRecordFamilyHistory: any }>(url, {
      withCredentials: true
    }).pipe(
      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if (error.status === 850) {
          errorMessage = error.error.message;
        }

        return errorMessage;
      })
    );

  }
}
