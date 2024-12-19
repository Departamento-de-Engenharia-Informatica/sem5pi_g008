import json from "../../appsettings.json"
import {Injectable} from '@angular/core';
import {HttpClient, HttpErrorResponse} from '@angular/common/http';
import {catchError, throwError} from 'rxjs';
import {MedicalConditionDTO} from '../../DTOs/general/medicalConditionDTO';

@Injectable ({
  providedIn: 'root',
})

export class MedicalConditionService {

  private apiUrl = json.backend2ApiUrl + '/medicalConditions';

  constructor(private http: HttpClient) {
  }

  addMedicalCondition(medicalConditionDTO: MedicalConditionDTO) {

    return this.http.post<string>(this.apiUrl, medicalConditionDTO, {
      withCredentials: true,
    }).pipe(
      catchError((error: HttpErrorResponse) => {

        console.log(error.status);
        console.log(error.error.message);
        console.log(error.error);
        console.log(error.message);
        console.log(error);


        let errorMessage = 'An unknown error occurred.';

        return throwError(errorMessage);
      })
    );
  }
}


