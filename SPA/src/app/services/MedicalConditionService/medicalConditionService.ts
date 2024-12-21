import {Injectable} from '@angular/core';
import json from "../../appsettings.json"
import {HttpClient, HttpErrorResponse} from '@angular/common/http';
import {catchError, Observable, throwError} from 'rxjs';
import {MedicalCondition} from '../../Domain/MedicalCondition';
import {MedicalConditionMapper} from '../../DTOs/mappers/medicalConditionMapper';

@Injectable ({
  providedIn: 'root',
})

export class MedicalConditionService {

  private apiUrl = json.backend2ApiUrl + '/medicalConditions';

  constructor(private http: HttpClient) {
  }

  addMedicalCondition(medicalCondition: MedicalCondition): Observable<string> {

    const medicalConditionDTO = MedicalConditionMapper.domainToBackendDto(medicalCondition);

    return this.http.post<string>(this.apiUrl, medicalConditionDTO, {
      withCredentials: true,
  }).pipe(

      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if(error.status >= 805 && error.status <= 809) {
          errorMessage = error.error.message;
        }

        if(error.status === 500) {
          errorMessage = error.error.message;
        }

        return throwError(errorMessage);
      })
    );
  }
}


